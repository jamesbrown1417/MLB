# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(tidyjson)

# Fix team names function
source("Functions/fix_team_names.R")

# Get Rosters
MLB_2025_Active_Rosters <- read_rds("Data/MLB_2025_Active_Rosters.rds")

# Get Pitchers
pitchers <-
  MLB_2025_Active_Rosters |>
  filter(position_name == "Pitcher") |> 
  select(person_full_name, team_name) |> 
  # separate full name into given names and last name
  separate(person_full_name, c("given_name", "last_name"), sep = " ", remove = FALSE) |> 
  mutate(join_name = paste(substr(given_name, 1, 1), last_name, sep = ". "))

# Batters
batters <-
  MLB_2025_Active_Rosters |>
  filter(position_name != "Pitcher") |> 
  select(person_full_name, team_name) |> 
  # separate full name into given names and last name
  separate(person_full_name, c("given_name", "last_name"), sep = " ", remove = FALSE) |> 
  mutate(join_name = paste(substr(given_name, 1, 1), last_name, sep = ". "))

pointsbet_main <- function() {
  # URL of website
  pointsbet_url = "https://api.au.pointsbet.com/api/v2/competitions/112658/events/featured?includeLive=false"
  
  # Make request and get response
  pointsbet_response <-
    request(pointsbet_url) |>
    req_perform() |>
    resp_body_json()
  
  # List of matches and data
  events <- pointsbet_response$events
  
  # Loop through to get all data--------------------------------------------------
  
  # Create empty vectors
  match_names <- c()
  match_starts_at <- c()
  home_teams <- c()
  away_teams <- c()
  event_names <- c()
  outcome_names <- c()
  outcome_prices <- c()
  keys <- c()
  
  # Loop through events
  for (match in events) {
    for (market in match$specialFixedOddsMarkets) {
      for (outcome in market$outcomes) {
        # Append data to vectors
        match_names <- c(match_names, match$name)
        match_starts_at <- c(match_starts_at, match$startsAt)
        home_teams <- c(home_teams, match$homeTeam)
        away_teams <- c(away_teams, match$awayTeam)
        event_names <- c(event_names, market$eventName)
        outcome_names <- c(outcome_names, outcome$name)
        outcome_prices <- c(outcome_prices, outcome$price)
        keys <- c(keys, match$key)
      }
    }
  }
  
  # Output tibble
  pointsbet_data <-
    tibble(
      match = match_names,
      start_time = match_starts_at,
      home_team = home_teams,
      away_team = away_teams,
      event = event_names,
      outcome = outcome_names,
      price = outcome_prices
    ) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    relocate(match, .before = start_time)
  
  #===============================================================================
  # Head to head markets
  #===============================================================================
  
  # Filter to head to head markets
  pointsbet_data_h2h <-
    pointsbet_data |>
    filter(event == "Moneyline")
  
  # Home Teams
  pointsbet_data_h2h_home <-
    pointsbet_data_h2h |>
    filter(home_team == outcome) |>
    select(match,
           start_time,
           market = event,
           home_team,
           home_win = price)
  
  # Away Teams
  pointsbet_data_h2h_away <-
    pointsbet_data_h2h |>
    filter(away_team == outcome) |>
    select(match,
           start_time,
           market = event,
           away_team,
           away_win = price)
  
  # Combine
  pointsbet_h2h <-
    full_join(
      pointsbet_data_h2h_home,
      pointsbet_data_h2h_away,
      by = c("match", "start_time", "market")
    ) |>
    mutate(market = "Head To Head") |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |> 
    mutate(match = paste(home_team, "v", away_team)) |> 
    select(match,
           start_time,
           market_name = market,
           home_team,
           home_win,
           away_team,
           away_win) |>
    mutate(margin = round((1 / home_win + 1 / away_win), digits = 3)) |>
    mutate(agency = "Pointsbet") |> 
    mutate(start_time =  with_tz(as_datetime(start_time), tzone = "Australia/Adelaide")) |> 
    # Remove time zone but keep the time
    mutate(start_time = as.character(start_time))
    
  # Write to csv
  write_csv(pointsbet_h2h, "Data/scraped_odds/pointsbet_h2h.csv")
  
#===============================================================================
# Player Props
#===============================================================================  

  # Get unique keys
  keys <- unique(keys)
  
  # Get each match's api page
  match_urls <-
    paste0("https://api.au.pointsbet.com/api/mes/v3/events/", keys)
  
  # Create a function that gets the player props from each URL
  get_player_props <- function(url) {
    # Make request and get response
    pointsbet_response <-
      request(url) |>
      req_perform() |>
      resp_body_json()
    
    # Loop through to get prop data---------------------------------------------
    
    # Create empty vectors
    match_names <- c()
    market_names <- c()
    outcome_names <- c()
    outcome_types <- c()
    outcome_prices <- c()
    event_key <- c()
    market_key <- c()
    outcome_key <- c()
    
    # Loop through events
    for (market in pointsbet_response$fixedOddsMarkets) {
      for (outcome in market$outcomes) {
        # Append data to vectors
        match_names <- c(match_names, pointsbet_response$name)
        
        if (!is.null(market$name)) {
          market_names <- c(market_names, market$name)
        } else {
          market_names <- c(market_names, NA)
        }
        
        if (!is.null(outcome$name)) {
          outcome_names <- c(outcome_names, outcome$name)
        } else {
          outcome_names <- c(outcome_names, NA)
        }
        
        if (!is.null(outcome$outcomeType)) {
          outcome_types <- c(outcome_types, outcome$outcomeType)
        } else {
          outcome_types <- c(outcome_types, NA)
        }
        
        if (!is.null(outcome$price)) {
          outcome_prices <- c(outcome_prices, outcome$price)
        } else {
          outcome_prices <- c(outcome_prices, NA)
        }
        
        event_key <- c(event_key, pointsbet_response$key)
        
        if (!is.null(market$key)) {
          market_key <- c(market_key, market$key)
        } else {
          market_key <- c(market_key, NA)
        }
        
        if (!is.null(outcome$key)) {
          outcome_key <- c(outcome_key, outcome$key)
        } else {
          outcome_key <- c(outcome_key, NA)
        }
      }
    }
    
    # Output tibble
    tibble(
      match = match_names,
      market = market_names,
      outcome = outcome_names,
      outcome_type = outcome_types,
      price = outcome_prices,
      EventKey = event_key,
      MarketKey = market_key,
      OutcomeKey = outcome_key
    )
  }
  
  # Map function to each URL
  pointsbet_data_player_props <- map_df(match_urls, get_player_props)
  
  # Helper function to correct common player names
  correct_player_names <- function(player_name) {
    case_when(
      player_name == "Yilber Diaz" ~ "Yilber Diaz",
      .default = player_name
    )
  }
  
  #===============================================================================
  # Batter Hits
  #===============================================================================
  
  # Batter Hits alternative ----------------------------------------------------
  
  # Filter list to player hits
  pointsbet_player_hits_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Alternate Hits")) |>
    mutate(line = str_extract(outcome, "[0-9]{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(match = str_replace(match, "@", "v")) |>
    mutate(outcome = str_remove(outcome, " \\d+\\+$")) |>
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |>
    mutate(outcome = correct_player_names(outcome)) |>
    left_join(batters[, c("person_full_name", "team_name")], by = c("outcome" = "person_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Batter Hits",
      player_name = outcome,
      player_team = team_name,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Get initials from alternate hits
  player_hits_pb_names <-
    pointsbet_player_hits_lines |> 
    select(player_name, player_team) |> 
    separate(player_name, into = c("player_first_name", "player_last_name"), sep = "\\ ", remove = FALSE) |>
    mutate(player_first_initial = substr(player_first_name, 1, 1)) |> 
    mutate(player_join_name = paste(player_first_initial, player_last_name, sep = ". ")) |> 
    distinct(player_join_name, player_name, player_team)
    
  # Player hits over / under----------------------------------------------------
  
  # Filter list to player hits over under
  pointsbet_player_hits_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Player Hits"))
  
  # Get Overs
  pointsbet_player_hits_over <-
    pointsbet_player_hits_over_under |>
    filter(str_detect(outcome, "Over")) |>
    mutate(outcome) |>
    separate(outcome, into = c("player_name_full", "line"), sep = " Over ") |>
    mutate(line = as.numeric(line)) |>
    mutate(match = str_replace(match, "@", "v")) |>
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |>
    mutate(player_name_full = correct_player_names(player_name_full)) |>
    left_join(player_hits_pb_names, by = c("player_name_full" = "player_join_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Player Hits",
      player_name,
      player_team,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Get Unders
  pointsbet_player_hits_under <-
    pointsbet_player_hits_over_under |>
    filter(str_detect(outcome, "Under")) |>
    mutate(outcome) |>
    separate(outcome, into = c("player_name_full", "line"), sep = " Under ") |>
    mutate(line = as.numeric(line)) |>
    mutate(match = str_replace(match, "@", "v")) |>
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |>
    mutate(player_name_full = correct_player_names(player_name_full)) |>
    left_join(player_hits_pb_names, by = c("player_name_full" = "player_join_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Player Hits",
      player_name,
      player_team,
      opposition_team,
      line,
      under_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Combine overs and unders
  pointsbet_player_hits_over_under <-
    pointsbet_player_hits_over |>
    left_join(pointsbet_player_hits_under) |>
    select(
      match,
      home_team,
      away_team,
      market_name,
      player_name,
      player_team,
      opposition_team,
      line,
      over_price,
      under_price,
      agency,
      contains("Key")
    )
  
  #===============================================================================
  # Batter RBIs
  #===============================================================================
  
  # Batter RBIs alternative ----------------------------------------------------
  
  # Filter list to player RBIs
  pointsbet_player_rbis_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Alternate Runs Batted In")) |>
    mutate(line = str_extract(outcome, "[0-9]{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(match = str_replace(match, "@", "v")) |>
    mutate(outcome = str_remove(outcome, " \\d+\\+$")) |>
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |>
    mutate(outcome = correct_player_names(outcome)) |>
    left_join(batters[, c("person_full_name", "team_name")], by = c("outcome" = "person_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Batter RBIs",
      player_name = outcome,
      player_team = team_name,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Get initials from alternate RBIs
  player_rbis_pb_names <-
    pointsbet_player_rbis_lines |> 
    select(player_name, player_team) |> 
    separate(player_name, into = c("player_first_name", "player_last_name"), sep = "\\ ", remove = FALSE) |>
    mutate(player_first_initial = substr(player_first_name, 1, 1)) |> 
    mutate(player_join_name = paste(player_first_initial, player_last_name, sep = ". ")) |> 
    distinct(player_join_name, player_name, player_team)
  
  # Player RBIs over / under----------------------------------------------------
  
  # Filter list to player RBIs over under
  pointsbet_player_rbis_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Player Runs Batted In"))
  
  # Get Overs
  pointsbet_player_rbis_over <-
    pointsbet_player_rbis_over_under |>
    filter(str_detect(outcome, "Over")) |>
    mutate(outcome) |>
    separate(outcome, into = c("player_name_full", "line"), sep = " Over ") |>
    mutate(line = as.numeric(line)) |>
    mutate(match = str_replace(match, "@", "v")) |>
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |>
    mutate(player_name_full = correct_player_names(player_name_full)) |>
    left_join(player_rbis_pb_names, by = c("player_name_full" = "player_join_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Player RBIs",
      player_name,
      player_team,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Get Unders
  pointsbet_player_rbis_under <-
    pointsbet_player_rbis_over_under |>
    filter(str_detect(outcome, "Under")) |>
    mutate(outcome) |>
    separate(outcome, into = c("player_name_full", "line"), sep = " Under ") |>
    mutate(line = as.numeric(line)) |>
    mutate(match = str_replace(match, "@", "v")) |>
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |>
    mutate(player_name_full = correct_player_names(player_name_full)) |>
    left_join(player_rbis_pb_names, by = c("player_name_full" = "player_join_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Player RBIs",
      player_name,
      player_team,
      opposition_team,
      line,
      under_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Combine overs and unders
  pointsbet_player_rbis_over_under <-
    pointsbet_player_rbis_over |>
    left_join(pointsbet_player_rbis_under) |>
    select(
      match,
      home_team,
      away_team,
      market_name,
      player_name,
      player_team,
      opposition_team,
      line,
      over_price,
      under_price,
      agency,
      contains("Key")
    )

  #===============================================================================
  # Pitcher Strikeouts
  #===============================================================================
  
  # Pitcher Strikeouts alternative ----------------------------------------------------
  
  # Filter list to pitcher strikeouts
  pointsbet_pitcher_strikeouts_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Alternate Pitcher Strikeouts")) |>
    mutate(line = str_extract(outcome, "[0-9]{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(match = str_replace(match, "@", "v")) |>
    mutate(outcome = str_remove(outcome, " \\d+\\+$")) |>
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |>
    mutate(outcome = correct_player_names(outcome)) |>
    left_join(pitchers[, c("person_full_name", "team_name")], by = c("outcome" = "person_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Pitcher Strikeouts",
      player_name = outcome,
      player_team = team_name,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Get initials from alternate strikeouts
  pitcher_strikeouts_pb_names <-
    pointsbet_pitcher_strikeouts_lines |> 
    select(player_name, player_team) |> 
    separate(player_name, into = c("player_first_name", "player_last_name"), sep = "\\ ", remove = FALSE) |>
    mutate(player_first_initial = substr(player_first_name, 1, 1)) |> 
    mutate(player_join_name = paste(player_first_initial, player_last_name, sep = ". ")) |> 
    distinct(player_join_name, player_name, player_team)
  
  # Pitcher strikeouts over / under----------------------------------------------------
  
  # Filter list to pitcher strikeouts over under
  pointsbet_pitcher_strikeouts_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "^Pitcher Strikeouts"))
  
  # Get Overs
  pointsbet_pitcher_strikeouts_over <-
    pointsbet_pitcher_strikeouts_over_under |>
    filter(str_detect(outcome, "Over")) |>
    mutate(outcome) |>
    separate(outcome, into = c("player_name_full", "line"), sep = " Over ") |>
    mutate(line = as.numeric(line)) |>
    mutate(match = str_replace(match, "@", "v")) |>
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |>
    mutate(player_name_full = correct_player_names(player_name_full)) |>
    left_join(pitcher_strikeouts_pb_names, by = c("player_name_full" = "player_join_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Pitcher Strikeouts",
      player_name,
      player_team,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Get Unders
  pointsbet_pitcher_strikeouts_under <-
    pointsbet_pitcher_strikeouts_over_under |>
    filter(str_detect(outcome, "Under")) |>
    mutate(outcome) |>
    separate(outcome, into = c("player_name_full", "line"), sep = " Under ") |>
    mutate(line = as.numeric(line)) |>
    mutate(match = str_replace(match, "@", "v")) |>
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |>
    mutate(player_name_full = correct_player_names(player_name_full)) |>
    left_join(pitcher_strikeouts_pb_names, by = c("player_name_full" = "player_join_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Pitcher Strikeouts",
      player_name,
      player_team,
      opposition_team,
      line,
      under_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Combine overs and unders
  pointsbet_pitcher_strikeouts_over_under <-
    pointsbet_pitcher_strikeouts_over |>
    left_join(pointsbet_pitcher_strikeouts_under) |>
    select(
      match,
      home_team,
      away_team,
      market_name,
      player_name,
      player_team,
      opposition_team,
      line,
      over_price,
      under_price,
      agency,
      contains("Key")
    )  
      
  #===============================================================================
  # Batter Home Runs
  #===============================================================================
  
  # Player home runs over / under----------------------------------------------------
  
  # Filter list to player home runs over under
  pointsbet_player_hr_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Player Home Run"))
  
  # Get Overs
  pointsbet_player_hr_over <-
    pointsbet_player_hr_over_under |>
    filter(str_detect(outcome, "Over")) |>
    mutate(outcome) |>
    separate(outcome, into = c("player_name_full", "line"), sep = " Over ") |>
    mutate(line = as.numeric(line)) |>
    mutate(match = str_replace(match, "@", "v")) |>
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |>
    mutate(player_name_full = correct_player_names(player_name_full)) |>
    left_join(player_hits_pb_names, by = c("player_name_full" = "player_join_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Player Home Runs",
      player_name,
      player_team,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Combine overs and unders
  pointsbet_player_hr_over_under <-
    pointsbet_player_hr_over |>
    select(
      match,
      home_team,
      away_team,
      market_name,
      player_name,
      player_team,
      opposition_team,
      line,
      over_price,
      agency,
      contains("Key")
    ) |> 
    filter(!is.na(player_name))

  #===============================================================================
  # Write to CSV
  #===============================================================================
  
  # Hits
  pointsbet_player_hits_lines |>
    bind_rows(pointsbet_player_hits_over_under) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    select(
      "match",
      "home_team",
      "away_team",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "over_price",
      "under_price",
      "agency",
      "opposition_team",
      "EventKey",
      "MarketKey",
      "OutcomeKey",
    ) |>
    mutate(market_name = "Batter Hits") |>
    mutate(agency = "Pointsbet") |>
    write_csv("Data/scraped_odds/pointsbet_batter_hits.csv")
  
  # Home Runs
  pointsbet_player_hr_over_under |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    select(
      "match",
      "home_team",
      "away_team",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "over_price",
      "agency",
      "opposition_team",
      "EventKey",
      "MarketKey",
      "OutcomeKey",
    ) |>
    mutate(market_name = "Batter Home Runs") |>
    mutate(agency = "Pointsbet") |>
    write_csv("Data/scraped_odds/pointsbet_batter_home_runs.csv")
  
  # RBIs
  pointsbet_player_rbis_lines |>
    bind_rows(pointsbet_player_rbis_over_under) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    select(
      "match",
      "home_team",
      "away_team",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "over_price",
      "under_price",
      "agency",
      "opposition_team",
      "EventKey",
      "MarketKey",
      "OutcomeKey",
    ) |>
    mutate(market_name = "Batter RBIs") |>
    mutate(agency = "Pointsbet") |>
    write_csv("Data/scraped_odds/pointsbet_batter_rbis.csv")
  
  # Pitcher Strikeouts
  pointsbet_pitcher_strikeouts_lines |>
    bind_rows(pointsbet_pitcher_strikeouts_over_under) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    select(
      "match",
      "home_team",
      "away_team",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "over_price",
      "under_price",
      "agency",
      "opposition_team",
      "EventKey",
      "MarketKey",
      "OutcomeKey",
    ) |>
    mutate(market_name = "Pitcher Strikeouts") |>
    mutate(agency = "Pointsbet") |>
    write_csv("Data/scraped_odds/pointsbet_pitcher_strikeouts.csv")
}

pointsbet_main()