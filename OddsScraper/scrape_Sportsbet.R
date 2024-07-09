# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# URL of website
sportsbet_url = "https://www.sportsbet.com.au/betting/baseball/major-league-baseball"

# Get Rosters
MLB_2024_Active_Rosters <- read_rds("Data/MLB_2024_Active_Rosters.rds")

# Fix team names function
source("Functions/fix_team_names.R")

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

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

main_markets_function <- function() {
  
  # Get data from main market page
  matches <-
    sportsbet_url |> 
    read_html() |>
    html_nodes(".White_fqa53j6")
  
  # Function to get team names
  get_team_names <- function(match) {
    team_names <-
      match |>
      html_nodes(".participantText_fivg86r") |>
      html_text()
    
    # Home team and Away Team
    home_team <- team_names[2]
    away_team <- team_names[1]
    
    # Output
    tibble(home_team, away_team)
  }
  
  # Function to get odds
  get_odds <- function(match) {
    odds <-
      match |>
      html_nodes(".priceTextSize_frw9zm9") |>
      html_text() |>
      as.numeric()
    
    # Home team
    home_win <- odds[2]
    away_win <- odds[1]
    
    # Output
    tibble(home_win, away_win)
  }
  
  # Function to get start time
  get_start_time <- function(match) {
    start_time <-
      match |>
      html_nodes(".oneLine_f15ay66x") |>
      html_text()
    
    # Output
    tibble(start_time)
  }
  
  # Map functions to each match and combine together
  all_main_market_data <-
    bind_cols(
      map(matches, get_team_names) |> bind_rows(),
      map(matches, get_odds) |> bind_rows(),
      map(matches, get_start_time) |> bind_rows()
    )
  
  #===============================================================================
  # Head to Head markets---------------------------------------------------------#
  #===============================================================================
  
  sportsbet_h2h <-
    all_main_market_data |>
    mutate(home_team = str_remove(home_team, " \\(.*\\)")) |>
    mutate(away_team = str_remove(away_team, " \\(.*\\)")) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(market_name = "Head To Head") |>
    mutate(home_win = as.numeric(home_win)) |>
    mutate(away_win = as.numeric(away_win)) |>
    select(match,
           start_time,
           market_name,
           home_team,
           home_win,
           away_team,
           away_win) |>
    mutate(margin = round((1 / home_win + 1 / away_win), digits = 3)) |>
    mutate(agency = "Sportsbet") |>
    mutate(start_time = str_remove(start_time, ",")) |> 
    mutate(start_time = parse_date_time(start_time, orders = "AdBHM")) |> 
    # Reduce start time by 31 minutes
    mutate(start_time = start_time - 31 * 60)
  
  # Write to csv
  write_csv(sportsbet_h2h, "Data/scraped_odds/sportsbet_h2h.csv")
  
}

##%######################################################%##
#                                                          #
####                    Player Props                    ####
#                                                          #
##%######################################################%##

player_props_function <- function() {
  
  # Get data from main market page
  matches <-
    sportsbet_url |> 
    read_html() |>
    html_nodes(".White_fqa53j6")
  
  # Function to get team names
  get_team_names <- function(match) {
    team_names <-
      match |>
      html_nodes(".participantText_fivg86r") |>
      html_text()
    
    # Home team and Away Team
    home_team <- team_names[2]
    away_team <- team_names[1]
    
    # Output
    tibble(home_team, away_team)
  }
  
  # Function to get odds
  get_odds <- function(match) {
    odds <-
      match |>
      html_nodes(".priceTextSize_frw9zm9") |>
      html_text() |>
      as.numeric()
    
    # Home team
    home_win <- odds[2]
    away_win <- odds[1]
    
    # Output
    tibble(home_win, away_win)
  }
  
  # Function to get start time
  get_start_time <- function(match) {
    start_time <-
      match |>
      html_nodes(".oneLine_f15ay66x") |>
      html_text()
    
    # Output
    tibble(start_time)
  }
  
  # Map functions to each match and combine together
  all_main_market_data <-
    bind_cols(
      map(matches, get_team_names) |> bind_rows(),
      map(matches, get_odds) |> bind_rows(),
      map(matches, get_start_time) |> bind_rows()
    )
  
  # Function to get team names
  get_team_names <- function(match) {
    team_names <-
      match |>
      html_nodes(".participantText_fivg86r") |>
      html_text()
    
    # Home team and Away Team
    home_team <- team_names[2]
    away_team <- team_names[1]
    
    # Output
    tibble(home_team, away_team)
  }
  
  # Get match links
  match_links <-
    sportsbet_url |> 
    read_html() |>
    html_nodes(".linkMultiMarket_fcmecz0") |> 
    html_attr("href")
  
  # Get match IDs from links
  match_ids <-
    match_links |>
    str_extract("\\d{4,10}$") |>
    as.numeric()
  
  # Get data from main market page
  matches <-
    sportsbet_url |> 
    read_html() |>
    html_nodes(".White_fqa53j6")
  
  # Get team names that correspond to each match link
  team_names <-
    map_dfr(matches, get_team_names) |> 
    bind_cols("match_id" = match_ids) |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team))
  
  # Match Times
  sportsbet_match_times <-
    all_main_market_data |>
    mutate(home_team = str_remove(home_team, " \\(.*\\)")) |>
    mutate(away_team = str_remove(away_team, " \\(.*\\)")) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    select(match,
           start_time,
           home_team,
           away_team) |>
    mutate(start_time = str_remove(start_time, ",")) |> 
    mutate(start_time = parse_date_time(start_time, orders = "AdBHM")) |> 
    # Reduce start time by 31 minutes
    mutate(start_time = start_time - 31 * 60) |> 
    mutate(match_id = match_ids)
  
  # Match info links
  match_info_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/SportCard?displayWinnersPriceMkt=true&includeLiveMarketGroupings=true&includeCollection=true")
  
  # Pitcher links
  pitcher_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/725/Markets")
  
  # Batter links
  batter_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/726/Markets")
  
  # Totals markets
  totals_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/246/Markets")
  
  # Get IDs needed for SGM engine-------------------------------------------------
  read_prop_url_metadata <- function(url) {
    
    # Make request and get response
    sb_response <-
      request(url) |>
      req_perform() |> 
      resp_body_json()
    
    # Empty vectors to append to
    class_external_id = c()
    competition_external_id = c()
    event_external_id = c()
    
    # Append to vectors
    class_external_id = c(class_external_id, sb_response$classExternalId)
    competition_external_id = c(competition_external_id, sb_response$competitionExternalId)
    event_external_id = c(event_external_id, sb_response$externalId)
    
    # Output
    tibble(class_external_id,
           competition_external_id,
           event_external_id,
           url) |> 
      mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |> 
      rename(match_id = url) |> 
      mutate(match_id = as.numeric(match_id))
  }
  
  # Safe version that just returns NULL if there is an error
  safe_read_prop_metadata <- safely(read_prop_url_metadata, otherwise = NULL)
  
  # Map function to player points urls
  player_prop_metadata <-
    map(match_info_links, safe_read_prop_metadata)
  
  # Get just result part from output
  player_prop_metadata <-
    player_prop_metadata |>
    map("result") |>
    map_df(bind_rows)
  
  # Function to read a url and get the player props-------------------------------
  
  read_prop_url <- function(url) {
    
    # Make request and get response
    sb_response <-
      request(url) |>
      req_perform() |> 
      resp_body_json()
    
    # Empty vectors to append to
    prop_market_name = c()
    selection_name_prop = c()
    prop_market_selection = c()
    prop_market_price = c()
    player_id = c()
    market_id = c()
    handicap = c()
    
    # Loop through each market
    for (market in sb_response) {
      for (selection in market$selections) {
        
        # Append to vectors
        prop_market_name = c(prop_market_name, market$name)
        selection_name_prop = c(selection_name_prop, selection$name)
        prop_market_selection = c(prop_market_selection, selection$resultType)
        prop_market_price = c(prop_market_price, selection$price$winPrice)
        player_id = c(player_id, selection$externalId)
        market_id = c(market_id, market$externalId)
        if (is.null(selection$unformattedHandicap)) {
          selection$unformattedHandicap = NA
          handicap = c(handicap, selection$unformattedHandicap)
        } else {
          selection$unformattedHandicap = as.numeric(selection$unformattedHandicap)
          handicap = c(handicap, selection$unformattedHandicap)
        }
      }
    }
    
    # Output
    tibble(prop_market_name,
           selection_name_prop,
           prop_market_selection,
           prop_market_price,
           player_id,
           market_id,
           handicap,
           url)
  }
  
  # Safe version that just returns NULL if there is an error
  safe_read_prop_url <- safely(read_prop_url, otherwise = NULL)
  
  #=============================================================================
  # Match Totals
  #=============================================================================
  
  # Map function to totals urls
  totals_data <-
    map(totals_links, safe_read_prop_url)
  
  # Get just result part from output
  totals_data <-
    totals_data |>
    map("result") |>
    map_df(bind_rows)
  
  # Main Totals
  main_totals <-
    totals_data |>
    filter(prop_market_name %in% c("Total Runs", "Total Runs in Match", "Alternate Total Runs")) |> 
    mutate(market_name = "Total Runs") |> 
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |> 
    rename(match_id = url) |> 
    mutate(match_id = as.numeric(match_id)) |> 
    left_join(team_names, by = "match_id") |> 
    mutate(match = paste(home_team, "v", away_team)) |> 
    left_join(sportsbet_match_times |> select(-home_team, -away_team), by = c("match_id", "match"))
  
  # Overs
  over_totals <-
    main_totals |> 
    filter(str_detect(selection_name_prop, "Over")) |>
    mutate(line = str_extract(selection_name_prop, "[0-9]{1,2}\\.[0-9]{1,2}")) |> 
    mutate(line = as.numeric(line)) |>
    mutate(line = coalesce(line, handicap)) |> 
    arrange(match_id, line, prop_market_price) |>
    group_by(match_id, match, line) |>
    slice_head(n = 1) |> 
    ungroup() |> 
    transmute(
      match,
      start_time,
      home_team,
      away_team,
      market_name = "Total Runs Over/Under",
      match_id,
      line,
      over_price = prop_market_price,
      agency = "Sportsbet"
    )
  
  # Unders
  under_totals <-
    main_totals |> 
    filter(str_detect(selection_name_prop, "Under")) |>
    mutate(line = str_extract(selection_name_prop, "[0-9]{1,2}\\.[0-9]{1,2}")) |> 
    mutate(line = as.numeric(line)) |>
    mutate(line = coalesce(line, handicap)) |> 
    arrange(match_id, line, prop_market_price) |>
    group_by(match_id, match, line) |>
    slice_head(n = 1) |> 
    ungroup() |> 
    transmute(
      match,
      start_time,
      home_team,
      away_team,
      market_name = "Total Runs Over/Under",
      match_id,
      line,
      under_price = prop_market_price,
      agency = "Sportsbet"
    )
  
  # Combine
  totals_all <-
    over_totals |>
    left_join(under_totals) |> 
    select(-match_id) |> 
    relocate(under_price, .after = over_price)
  
  # Write to csv
  write_csv(totals_all, "Data/scraped_odds/sportsbet_total_runs.csv")
  
  #===============================================================================
  # Batter Hits
  #===============================================================================
  
  # Map function to batter hits urls
  batter_hits_data <-
    map(batter_links, safe_read_prop_url)
  
  # Get just result part from output
  batter_hits_data <-
    batter_hits_data |>
    map("result") |>
    map_df(bind_rows)
  
  # Add market name
  batter_hits_data <-
    batter_hits_data |>
    filter(str_detect(prop_market_name, "Qtr", negate = TRUE)) |>
    filter(str_detect(prop_market_name, "Hit")) |>
    mutate(market_name = "Player Hits") |> 
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |> 
    rename(match_id = url) |> 
    mutate(match_id = as.numeric(match_id)) |> 
    left_join(team_names, by = "match_id") |> 
    mutate(match = paste(home_team, "v", away_team)) |> 
    left_join(player_prop_metadata) |> 
    left_join(sportsbet_match_times |> select(-home_team, -away_team), by = c("match_id", "match"))
  
  # Get batter hits alternate lines---------------------------------------------
  batter_hits_alternate <-
    batter_hits_data |>
    mutate(prop_market_name = str_replace(prop_market_name, "A Hit", "1+ Hits")) |>
    filter(str_detect(prop_market_name, "Hits")) |>
    mutate(line = str_extract(prop_market_name, "\\d{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    rename(player_name = selection_name_prop) |>
    mutate(player_name = case_when(
      player_name == "Tomas Nido" ~ "Tomás Nido",
      player_name == "Luis Garcia" ~ "Luis García Jr.",
      player_name == "Adolis Garcia" ~ "Adolis García",
      player_name == "Jeremy Pena" ~ "Jeremy Peña",
      player_name == "Mauricio Dubon" ~ "Mauricio Dubón",
      player_name == "Eugenio Suarez" ~ "Eugenio Suárez",
      player_name == "Elias Diaz" ~ "Elias Díaz",
      player_name == "Javier Baez" ~ "Javier Báez",
      player_name == "Wenceel Perez" ~ "Wenceel Pérez",
      player_name == "Ryan OHearn" ~ "Ryan O'Hearn",
      player_name == "Logan OHoppe" ~ "Logan O'Hoppe",
      player_name == "Josh H. Smith" ~ "Josh Smith",
      player_name == "Ivan Herrera" ~ "Iván Herrera",
      .default = player_name
    )) |> 
    left_join(batters, by = c("player_name" = "person_full_name")) |> 
    mutate(opposition_team = if_else(team_name == home_team, away_team, home_team)) |>
    relocate(match, .before = player_name) |>
    transmute(
      match,
      start_time,
      home_team,
      away_team,
      market_name,
      player_name,
      player_team = team_name,
      opposition_team,
      line,
      over_price = prop_market_price,
      agency = "Sportsbet",
      class_external_id,
      competition_external_id,
      event_external_id,
      market_id,
      player_id
    )
  
  #===============================================================================
  # Pitcher Strikeouts
  #===============================================================================
  
  # Map function to pitcher strikeouts urls
  pitcher_strikeouts_data <-
    map(pitcher_links, safe_read_prop_url)
  
  # Get just result part from output
  pitcher_strikeouts_data <-
    pitcher_strikeouts_data |>
    map("result") |>
    map_df(bind_rows)
  
  # Add market name
  pitcher_strikeouts_data <-
    pitcher_strikeouts_data |>
    filter(str_detect(prop_market_name, "Qtr", negate = TRUE)) |>
    mutate(market_name = "Pitcher Strikeouts") |> 
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |> 
    rename(match_id = url) |> 
    mutate(match_id = as.numeric(match_id)) |> 
    left_join(team_names, by = "match_id") |> 
    mutate(match = paste(home_team, "v", away_team)) |> 
    left_join(player_prop_metadata) |> 
    left_join(sportsbet_match_times |> select(-home_team, -away_team), by = c("match_id", "match"))
  
  # Get pitcher strikeouts alternate lines---------------------------------------------

  pitcher_strikeouts_alternate <-
    pitcher_strikeouts_data |>
    filter(str_detect(prop_market_name, "Alt Strikeouts$")) |>
    mutate(line = str_extract(selection_name_prop, "\\d{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    rename(player_name = selection_name_prop) |>
    mutate(player_name = str_remove_all(player_name, " \\d+\\+ Strikeouts$")) |>
    mutate(
      player_name =
        case_when(
          player_name == "Reynaldo Lopez" ~ "Reynaldo López",
          player_name == "Ranger Suarez" ~ "Ranger Suárez",
          .default = player_name)) |>
    rename(over_price = prop_market_price) |>
    left_join(pitchers, by = c("player_name" = "person_full_name")) |> 
    mutate(opposition_team = if_else(team_name == home_team, away_team, home_team)) |>
    relocate(match, .before = player_name) |>
    transmute(
      match,
      start_time,
      home_team,
      away_team,
      market_name = "Pitcher Strikeouts",
      player_name,
      player_team = team_name,
      opposition_team,
      line,
      over_price,
      agency = "Sportsbet",
      class_external_id,
      competition_external_id,
      event_external_id,
      market_id,
      player_id
    )
  
  # Get pitcher strikeouts over / under -----------------------------------------------
  
  pitcher_strikeouts_over <-
    pitcher_strikeouts_data |> 
    filter(str_detect(selection_name_prop, "Over")) |>
    separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Over ", remove = FALSE) |>
    mutate(line_2 = as.numeric(line_2)) |>
    mutate(handicap = coalesce(handicap, line_2)) |>
    rename(player_name = selection_name_prop) |> 
    mutate(player_name = str_remove(player_name, " Over")) |>
    mutate(player_name = str_remove(player_name, " \\d+\\.\\d+")) |>
    rename(line = handicap) |>  
    mutate(
      player_name =
        case_when(
          player_name == "Reynaldo Lopez" ~ "Reynaldo López",
          player_name == "Ranger Suarez" ~ "Ranger Suárez",
          player_name == "Yilber Diaz" ~ "Yilber Díaz",
          .default = player_name)) |>
    rename(over_price = prop_market_price) |>
    left_join(pitchers, by = c("player_name" = "person_full_name")) |> 
    mutate(opposition_team = if_else(team_name == home_team, away_team, home_team)) |>
    relocate(match, .before = player_name) |>
    transmute(
      match,
      start_time,
      home_team,
      away_team,
      market_name = "Pitcher Strikeouts",
      player_name,
      player_team = team_name,
      opposition_team,
      line,
      over_price,
      agency = "Sportsbet",
      class_external_id,
      competition_external_id,
      event_external_id,
      market_id,
      player_id
    )
  
  pitcher_strikeouts_under <-
    pitcher_strikeouts_data |> 
    filter(str_detect(selection_name_prop, "Under")) |> 
    separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Under ", remove = FALSE) |>
    mutate(line_2 = as.numeric(line_2)) |>
    mutate(handicap = coalesce(handicap, line_2)) |>
    rename(player_name = selection_name_prop) |> 
    mutate(player_name = str_remove(player_name, " Under")) |>
    mutate(player_name = str_remove(player_name, " \\d+\\.\\d+")) |>
    rename(line = handicap) |> 
    mutate(
      player_name =
        case_when(
          player_name == "Reynaldo Lopez" ~ "Reynaldo López",
          player_name == "Ranger Suarez" ~ "Ranger Suárez",
          player_name == "Yilber Diaz" ~ "Yilber Díaz",
          .default = player_name)) |>
    rename(under_price = prop_market_price) |>
    left_join(pitchers, by = c("player_name" = "person_full_name")) |> 
    mutate(opposition_team = if_else(team_name == home_team, away_team, home_team)) |>
    relocate(match, .before = player_name) |>
    transmute(
      match,
      start_time,
      home_team,
      away_team,
      market_name = "Pitcher Strikeouts",
      player_name,
      player_team = team_name,
      opposition_team,
      line,
      under_price,
      agency = "Sportsbet",
      class_external_id,
      competition_external_id,
      event_external_id,
      market_id,
      player_id_unders = player_id
    )
  
  # Combine
  pitcher_strikeouts_over_under <-
    pitcher_strikeouts_over |> 
    left_join(pitcher_strikeouts_under)
  
  #===============================================================================
  # Write to CSV
  #===============================================================================
  
  # Hits
  batter_hits_alternate |>
    # bind_rows(batter_hits_over_under) |>
    select(
      "match",
      "start_time",
      "home_team",
      "away_team",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "over_price",
      "agency",
      "opposition_team",
      "class_external_id",
      "competition_external_id",
      "event_external_id",
      "market_id",
      "player_id"
    ) |>
    mutate(market_name = "Batter Hits") |>
    mutate(agency = "Sportsbet") |>
    write_csv("Data/scraped_odds/sportsbet_batter_hits.csv")
  
  # Strikeouts
  pitcher_strikeouts_alternate |> 
    bind_rows(pitcher_strikeouts_over_under) |>
    select(
      "match",
      "start_time",
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
      "class_external_id",
      "competition_external_id",
      "event_external_id",
      "market_id",
      "player_id",
      "player_id_unders"
    ) |>
    mutate(market_name = "Pitcher Strikeouts") |>
    mutate(agency = "Sportsbet") |>
    write_csv("Data/scraped_odds/sportsbet_pitcher_strikeouts.csv")
}

##%######################################################%##
#                                                          #
####                Run functions safely                ####
#                                                          #
##%######################################################%##

safe_main_markets <- safely(main_markets_function, otherwise = NULL)
safe_player_props <- safely(player_props_function, otherwise = NULL)

safe_main_markets()
safe_player_props()
