# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(glue)

# URL to get responses
betright_url = "https://next-api.betright.com.au/Sports/Category?categoryId=2077"

# Fix team names function
source("Functions/fix_team_names.R")

# Make request and get response
betright_response <-
  request(betright_url) |>
  req_perform() |> 
  resp_body_json()

# Get matches
matches <- betright_response$masterCategories[[1]]$categories[[1]]$masterEvents

# Keep only matches
matches <-
  matches |> 
  keep(~ .x$masterEventClassName == "Matches")

# Function to extract market info from response---------------------------------
get_market_info <- function(market) {
  
  # Market info
  markets_name = market$eventName
  market_propositions = market$outcomeName
  market_prices = market$price
  
  # Output Tibble
  tibble(market = markets_name,
         propositions = market_propositions,
         prices = market_prices)
}


# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
  # Match info
  match_name = matches$masterEventName
  match_start_time = matches$minAdvertisedStartTime
  match_id = matches$masterEventId
  
  # Market info
  market_info = map(matches$markets, get_market_info) |> bind_rows()
  
  # Output Tibble
  tibble(
    match = match_name,
    match_id = match_id,
    start_time = match_start_time,
    market_name = market_info$market,
    propositions = market_info$propositions,
    prices = market_info$prices
  )
}

# Map functions to data
all_betright_markets <-
  map(matches, get_match_info) |> bind_rows()

#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
  all_betright_markets |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  filter(str_detect(market_name, "Money Line")) |> 
  mutate(market_name = "Head To Head") |> 
  group_by(match) |> 
  filter(row_number() == 1) |> 
  rename(home_win = prices) |> 
  select(-propositions)

# Away teams
away_teams <-
  all_betright_markets |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  filter(str_detect(market_name, "Money Line")) |> 
  mutate(market_name = "Head To Head") |>
  group_by(match) |> 
  filter(row_number() == 2) |> 
  rename(away_win = prices) |> 
  select(-propositions)

# Combine
betright_head_to_head_markets <-
  home_teams |>
  left_join(away_teams) |> 
  select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
  mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
  mutate(agency = "BetRight")

# Fix team names and start time
betright_head_to_head_markets <-
  betright_head_to_head_markets |> 
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(start_time =  with_tz(as_datetime(start_time), tzone = "Australia/Adelaide")) |> 
  # Remove time zone but keep the time
  mutate(start_time = as.character(start_time))

# Write to csv
write_csv(betright_head_to_head_markets, "Data/scraped_odds/betright_h2h.csv")

#===============================================================================
# Totals 
#===============================================================================

# Totals URL
totals_url = "https://next-api.betright.com.au/Sports/MasterEvent?masterEventId=1217066&groupTypeCode=G26"

#===============================================================================
# Lines 
#===============================================================================

# Lines URL
lines_url = "https://next-api.betright.com.au/Sports/MasterEvent?masterEventId=1217066&groupTypeCode=G27"

#===============================================================================
# Hits
#===============================================================================

# Hits URL
hits_url = "https://next-api.betright.com.au/Sports/MasterEvent?masterEventId=1217066&groupTypeCode=G398"

#===============================================================================
# Pitcher
#===============================================================================

# Pitcher URL
pitcher_url = "https://next-api.betright.com.au/Sports/MasterEvent?masterEventId=1217066&groupTypeCode=G570"
