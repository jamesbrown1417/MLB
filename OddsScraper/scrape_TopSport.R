# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(future)
library(furrr)

plan(multisession)

# Fix team names function
source("Functions/fix_team_names.R")

# URL of website
topsport_url = "https://www.topsport.com.au/Sport/Baseball/MLB_Match/Matches"

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

# Get data from main market page
main_markets <-
  topsport_url |> 
  read_html() |>
  html_nodes(".marketTable") |> 
  html_table()

#===============================================================================
# Use rvest to get additional market information-------------------------------#
#===============================================================================

# Get links to other markets
topsport_other_markets <-
  topsport_url |>
  read_html() |>
  html_nodes("dd") |> 
  html_attr("data-compurl")

# Remove NA
topsport_other_markets <- topsport_other_markets[!is.na(topsport_other_markets)]

# Remove ?issubcomp=true
topsport_other_markets <- str_remove(topsport_other_markets, "\\?issubcomp=true")

# Add base url
topsport_other_markets <- paste0("https://www.topsport.com.au", topsport_other_markets)

#===============================================================================
# Head to Head markets---------------------------------------------------------#
#===============================================================================

head_to_head_main <- function() {
  
  # Function to get head to head data--------------------------------------------#
  get_h2h <- function(market_table) {
    
    # Home Team Data
    home_info <- market_table[2, 1:2]
    names(home_info) <- c("home_team", "home_win")
    
    # Away Team Data
    away_info <- market_table[3, 1:2]
    names(away_info) <- c("away_team", "away_win")
    
    # Match Start Time
    match_start_time <- market_table[1, 1]
    names(match_start_time) <- "start_time"
    
    # Combine together into one table
    bind_cols(home_info, away_info, match_start_time)
    
  }
  
  # Map function to main markets list
  topsport_h2h <- future_map(main_markets, get_h2h) |> bind_rows()
  
  # Fix names
  topsport_h2h <-
    topsport_h2h |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
    mutate(market_name = "Head To Head") |>
    mutate(home_win = as.numeric(home_win)) |>
    mutate(away_win = as.numeric(away_win)) |>
    select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "TopSport") |> 
    mutate(start_time = dmy_hm(start_time)) |>
    mutate(start_time = start_time - minutes(30)) |> 
    filter(!is.na(home_win))
  
  # Write to csv
  write_csv(topsport_h2h, "Data/scraped_odds/topsport_h2h.csv")
}

head_to_head_main()