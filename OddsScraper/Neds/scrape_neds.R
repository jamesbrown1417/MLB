# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# Get Rosters
MLB_2024_Active_Rosters <- read_rds("Data/MLB_2024_Active_Rosters.rds")

# Get Pitchers
pitchers <-
  MLB_2024_Active_Rosters |>
  filter(position_name == "Pitcher") |> 
  select(person_full_name, team_name) |> 
  # separate full name into given names and last name
  separate(person_full_name, c("given_name", "last_name"), sep = " ", remove = FALSE) |> 
  mutate(join_name = paste(substr(given_name, 1, 1), last_name, sep = ". "))

# Batters
batters <-
  MLB_2024_Active_Rosters |>
  filter(position_name != "Pitcher") |> 
  select(person_full_name, team_name) |> 
  # separate full name into given names and last name
  separate(person_full_name, c("given_name", "last_name"), sep = " ", remove = FALSE) |> 
  mutate(join_name = paste(substr(given_name, 1, 1), last_name, sep = ". "))

# Fix team names function
source("Functions/fix_team_names.R")

#===============================================================================
# Get JSON for each match
#===============================================================================

# Read in df
df <- read_csv("OddsScraper/Neds/neds_mlb_match_urls.csv")

# Get match json files
json_match_files <- list.files("OddsScraper/Neds/", pattern = "^data_.*.json", full.names = TRUE)

event_json_list <- map(json_match_files, ~fromJSON(.x))

#===============================================================================
# Get the market information for each match
#===============================================================================

# Initialize empty vectors to store the market names and IDs for mapping
market_lookup_name <- character()
market_lookup_id <- character()

# Initialize empty vectors to store data
event_ids <- character()
entrants <- character()
market_id <- character()
match_names <- character()
handicaps <- numeric()
prices <- numeric()
start_time <- character()

# Loop through the entrants
for (i in seq_along(event_json_list)) {
    # match_name <- df$event_name[i]
      match <- event_json_list[[i]] 
    
    for (entrant in match$entrants) {
        entrants <- c(entrants, entrant$name)
        market_id <- c(market_id, entrant$market_id)
        event_ids <- c(event_ids,  event_json_list[[i]]$events[[1]]$id)
    } 

    
    # Loop through the markets
    for (market in match$markets) {
        market_lookup_name <- c(market_lookup_name, market$name)
        market_lookup_id <- c(market_lookup_id, market$id)
        
        if (is.null(market$handicap)) {
            handicaps <- c(handicaps, NA)
        } else {
            handicaps <- c(handicaps, market$handicap)
        }
    }
    
      
    # Loop through the prices
    for (price in match$prices) {
        fractional_odds <- price$odds$numerator / price$odds$denominator
        decimal_odds <- fractional_odds + 1
        prices <- c(prices, decimal_odds)
        start_time <- c(start_time, match$events[[1]]$advertised_start)
    }
}

# Create market lookup dataframe
market_lookup_df <- data.frame(market_id = market_lookup_id, market_name = market_lookup_name, handicaps = handicaps)

# Create market dataframe
market_df <- data.frame(event_id = event_ids, start_time = start_time, market_id = market_id, entrants = entrants, price = prices)

# Merge market lookup dataframe with market dataframe
market_df <- merge(market_df, market_lookup_df, by = 'market_id', all.x = TRUE)

# Reorder columns in market_df
market_df <- market_df |> select(event_id, start_time, market_name, entrants, handicaps, price)

# Add match names
market_df <-
  market_df |> 
  left_join(df[,c("event_name", "event_id")], by = c("event_id" = "event_id")) |> 
  relocate(event_name, .before = event_id) |> 
  rename(match_name = event_name) |> 
  select(-event_id)

# Convert start time to adelaide time (currently utc)
market_df <-
  market_df |> 
  mutate(start_time =  with_tz(as_datetime(start_time), tzone = "Australia/Adelaide")) |> 
  # Remove time zone but keep the time
  mutate(start_time = as.character(start_time))

##%######################################################%##
#                                                          #
####               Get Head to Head Data                ####
#                                                          #
##%######################################################%##

# Filter to only include head to head markets
h2h_data <-
market_df |> 
    filter(market_name == "Head To Head") |> 
    select(-market_name)

# Home teams
home_teams <-
    h2h_data |> 
    separate(match_name, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |> 
    filter(entrants == home_team) |> 
    select(match = match_name, start_time, home_team, home_win = price)

# Away teams
away_teams <-
    h2h_data |> 
    separate(match_name, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |> 
    filter(entrants == away_team) |> 
    select(match = match_name, start_time, away_team, away_win = price)

# Merge home and away teams
h2h_data <-
    home_teams |> 
    left_join(away_teams, by = c("match", "start_time")) |> 
    mutate(margin = round(1 / home_win + 1 / away_win, digits = 2)) |>
    mutate(agency = "Neds") |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
    mutate(market_name = "Head To Head") |> 
    select(match, start_time, market_name, home_team, away_team, home_win, away_win, margin, agency)

##%######################################################%##
#                                                          #
####                   Total Runs Data                  ####
#                                                          #
##%######################################################%##

# Filter to only include totals markets
totals_data <-
  market_df |> 
  filter(str_detect(market_name, "^Total")) |> 
  select(-market_name)

# Overs
totals_overs <-
  totals_data |> 
  filter(str_detect(entrants, "Over")) |> 
  mutate(line = str_remove(entrants, "Over ")) |> 
  mutate(line = str_remove(line, "\\(")) |> 
  mutate(line = str_remove(line, "\\)")) |>
  mutate(line = as.numeric(line)) |>
  select(match = match_name, start_time, over_price = price, line)

# Unders
totals_unders <-
  totals_data |> 
  filter(str_detect(entrants, "Under")) |> 
  mutate(line = str_remove(entrants, "Under ")) |> 
  mutate(line = str_remove(line, "\\(")) |> 
  mutate(line = str_remove(line, "\\)")) |>
  mutate(line = as.numeric(line)) |>  select(match = match_name, start_time, under_price = price, line)

# Merge overs and unders
totals_all <-
  totals_overs |> 
  left_join(totals_unders, by = c("match", "start_time", "line")) |> 
  mutate(margin = round(1 / over_price + 1 / under_price, digits = 2)) |>
  separate(match, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(agency = "Neds") |>
  mutate(market_name = "Total Runs Over/Under") |> 
  select(match, start_time, market_name, line, over_price, under_price, agency)

##%######################################################%##
#                                                          #
####                  Match Line Data                   ####
#                                                          #
##%######################################################%##



##%######################################################%##
#                                                          #
####                  Write out as CSV                  ####
#                                                          #
##%######################################################%##

h2h_data |> write_csv("Data/scraped_odds/neds_h2h.csv")
totals_all |> write_csv("Data/scraped_odds/neds_total_runs.csv")
