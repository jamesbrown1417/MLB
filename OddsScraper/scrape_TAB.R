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

# URL to get responses
tab_url = "https://api.beta.tab.com.au/v1/tab-info-service/sports/Baseball/competitions/Major%20League%20Baseball?homeState=SA&jurisdiction=SA"

# Fix team names function
source("Functions/fix_team_names.R")

main_tab <- function() {
  
  # Function to fetch and parse JSON with exponential backoff
  fetch_data_with_backoff <-
    function(url,
             delay = 1,
             max_retries = 5,
             backoff_multiplier = 2) {
      tryCatch({
        # Attempt to fetch and parse the JSON
        tab_response <-
          read_html_live(url) |>
          html_nodes("pre") %>%
          html_text() %>%
          fromJSON(simplifyVector = FALSE)
        
        # Return the parsed response
        return(tab_response)
      }, error = function(e) {
        if (max_retries > 0) {
          # Log the retry attempt
          message(sprintf("Error encountered. Retrying in %s seconds...", delay))
          
          # Wait for the specified delay
          Sys.sleep(delay)
          
          # Recursively call the function with updated parameters
          return(
            fetch_data_with_backoff(
              url,
              delay * backoff_multiplier,
              max_retries - 1,
              backoff_multiplier
            )
          )
        } else {
          # Max retries reached, throw an error
          stop("Failed to fetch data after multiple retries.")
        }
      })
    }
  
  tab_response <- fetch_data_with_backoff(tab_url)
  
  # # Get index element of competitions with name value "AFL"
  # names_list <- map(tab_response$competitions, "name")
  # index <- which(names_list == "Major League Baseball")
  # 
  # # Get the response
  # tab_response <- tab_response$competitions[[index]]
  
  # Function to extract market info from response---------------------------------
  get_market_info <- function(markets) {
    
    # Market info
    markets_name = markets$betOption
    market_propositions = markets$propositions
    
    # Output Tibble
    tibble(market = markets_name,
           propositions = market_propositions)
  }
  
  # Function to extract match info from response----------------------------------
  get_match_info <- function(matches) {
    # Match info
    match_name = matches$name
    match_start_time = matches$startTime
    
    # Market info
    market_info = map(matches$markets, get_market_info) |> bind_rows()
    
    # Output Tibble
    tibble(
      match = match_name,
      start_time = match_start_time,
      market_name = market_info$market,
      propositions = market_info$propositions
    )
  }
  
  # Map functions to data
  all_tab_markets <-
    map(tab_response$matches, get_match_info) |> bind_rows()
  
  # Expand list col into multiple cols
  all_tab_markets <-
    all_tab_markets |>
    unnest_wider(col = propositions, names_sep = "_") |>
    select(any_of(c("match",
                    "start_time",
                    "market_name")),
           prop_name = propositions_name,
           prop_id = propositions_id,
           price = propositions_returnWin) |> 
    # Remove anything in brackets from the match column
    mutate(match = str_remove(match, "\\(.*\\)")) |> 
    mutate(match = trimws(match)) |> 
    mutate(start_time =  with_tz(as_datetime(start_time), tzone = "Australia/Adelaide")) |> 
    # Remove time zone but keep the time
    mutate(start_time = as.character(start_time))
  
  #===============================================================================
  # Head to head markets
  #===============================================================================
  
  # Home teams
  home_teams <-
    all_tab_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Head To Head") |> 
    group_by(match) |> 
    filter(row_number() == 1) |> 
    rename(home_win = price) |> 
    select(-prop_name, -prop_id)
  
  # Away teams
  away_teams <-
    all_tab_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Head To Head") |> 
    group_by(match) |> 
    filter(row_number() == 2) |> 
    rename(away_win = price) |> 
    select(-prop_name, -prop_id)
  
  # Combine
  tab_head_to_head_markets <-
    home_teams |>
    left_join(away_teams) |> 
    select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "TAB")
  
  # Fix team names
  tab_head_to_head_markets <-
    tab_head_to_head_markets |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

  # Write to csv
  write_csv(tab_head_to_head_markets, "Data/scraped_odds/tab_h2h.csv")
  
  #===============================================================================
  # Match Line
  #===============================================================================
  
  # Home teams
  home_teams_match_lines <-
    all_tab_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Line") |> 
    group_by(match) |> 
    separate(prop_name, into = c("team", "line"), sep = "[\\+\\-]", remove = FALSE) |>
    # Get rid of trailing or leading whitespace
    mutate(team = str_trim(team)) |>
    filter(team == home_team) |>
    rename(home_price = price) |> 
    mutate(home_line = str_extract(prop_name, "-?[0-9]+\\.?[0-9]*")) |>
    mutate(home_line = as.numeric(home_line)) |>
    select(-prop_name, -prop_id, -team, -line) |> 
    ungroup() |> 
    mutate(away_line = -1*home_line)
  
  # Away teams
  away_teams_match_lines <-
    all_tab_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Line") |> 
    group_by(match) |> 
    separate(prop_name, into = c("team", "line"), sep = "[\\+\\-]", remove = FALSE) |>
    # Get rid of trailing or leading whitespace
    mutate(team = str_trim(team)) |>
    filter(team == away_team) |>
    rename(away_price = price) |> 
    mutate(away_line = str_extract(prop_name, "-?[0-9]+\\.?[0-9]*")) |>
    mutate(away_line = as.numeric(away_line)) |>
    select(-prop_name, -prop_id, -team, -line) |> 
    ungroup() |> 
    mutate(home_line = -1*away_line)
  
  # Combine
  tab_match_line_markets <-
    home_teams_match_lines |>
    left_join(away_teams_match_lines) |>
    select(match, start_time, market_name, home_team, home_line, home_price,away_team, away_line, away_price) |> 
    mutate(margin = round((1/home_price + 1/away_price), digits = 3)) |> 
    mutate(agency = "TAB")
  
  # Fix team names
  tab_match_line_markets <-
    tab_match_line_markets |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))
  
  # Write to csv
  write_csv(tab_match_line_markets, "Data/scraped_odds/tab_match_line.csv")
  
  #===============================================================================
  # Total Runs
  #===============================================================================
  
  # Total Runs - Over
  total_runs_over <-
    all_tab_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Total Runs Over/Under") |> 
    filter(str_detect(prop_name, "Over")) |> 
    rename(over_price = price) |> 
    mutate(line = str_extract(prop_name, "[0-9]+\\.?[0-9]*")) |>
    select(-prop_name, -prop_id)
  
  # Total Runs - Under
  total_runs_under <-
    all_tab_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Total Runs Over/Under") |> 
    filter(str_detect(prop_name, "Under")) |> 
    rename(under_price = price) |> 
    mutate(line = str_extract(prop_name, "[0-9]+\\.?[0-9]*")) |>
    select(-prop_name, -prop_id)
  
  # Combine
  tab_total_runs_markets <-
    total_runs_over |>
    left_join(total_runs_under) |>
    select(match, start_time, market_name, home_team, away_team, line, over_price, under_price) |> 
    mutate(margin = round((1/over_price + 1/under_price), digits = 3)) |> 
    mutate(agency = "TAB")

  # Fix team names
  tab_total_runs_markets <-
    tab_total_runs_markets |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
    select(-margin)
  
  # Write to csv
  write_csv(tab_total_runs_markets, "Data/scraped_odds/tab_total_runs.csv")
  
  #===============================================================================
  # Pitcher Strikeouts
  #===============================================================================
  
  # Filter to pitcher strikeout markets
  player_strikeouts_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "Pitcher Strikeouts"))
  
  # Alternate Player strikeouts
  alternate_player_strikeouts_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "Pitcher Strikeouts$"))
  
  alternate_player_strikeouts_markets <-
    alternate_player_strikeouts_markets |>
    mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
    mutate(player_name = str_remove(player_name, " \\d+\\+ SOs")) |>
    mutate(line = str_extract(prop_name, "\\d+")) |> 
    mutate(line = as.numeric(line) - 0.5) |> 
    transmute(match, market_name = "Pitcher Strikeouts", player_name, line, over_price = price, prop_id)
  
  # Combine
  tab_player_strikeouts_markets <-
    alternate_player_strikeouts_markets |>
    select(match, market_name, player_name, line, over_price, prop_id) |> 
    mutate(agency = "TAB")
  
  # Fix team names
  tab_player_strikeouts_markets <-
    tab_player_strikeouts_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
    mutate(player_name = case_when(
      player_name == "Reynaldo Lopez" ~ "Reynaldo López",
      player_name == "Ranger Suarez" ~ "Ranger Suárez",
      player_name == "Yilber Diaz" ~ "Yilber Díaz",
      .default = player_name
    )) |> 
    left_join(pitchers, by = c("player_name" = "join_name")) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) |>
    relocate(player_team, opposition_team, .after = player_name) |> 
    mutate(player_name = person_full_name) |> 
    select(-person_full_name, -given_name, -last_name)
  
  #===============================================================================
  # Player Home Runs
  #===============================================================================
  
  # Filter to player home runs markets
  player_home_runs_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "Home Run$"))
  
  # Alternate Player Home Runs
  alternate_player_home_runs_markets <-
    player_home_runs_markets |> 
    mutate(market_name = if_else(str_detect(market_name, "A Home Run"), "1+ Home Runs", market_name))
  
  # Extract player names
  player_home_runs_markets <-
    player_home_runs_markets |> 
    filter(str_detect(prop_name, "Over|Under")) |>
    mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
    mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
    mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
    mutate(line = as.numeric(line)) |>
    mutate(type = str_detect(prop_name, "Over|\\+")) |> 
    mutate(type = ifelse(type, "Over", "Under")) |> 
    mutate(line = if_else(market_name == "Alternate Player Home Runs", line - 0.5, line))
  
  alternate_player_home_runs_markets <-
    alternate_player_home_runs_markets |>
    mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
    mutate(line = str_extract(market_name, "\\d+")) |> 
    mutate(line = as.numeric(line) - 0.5) |> 
    transmute(match, market_name = "Player Home Runs", player_name, line, over_price = price, prop_id)
  
  # Over lines
  over_lines <-
    player_home_runs_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Home Runs") |>
    select(match, market_name, player_name, line, over_price = price, prop_id) |> 
    bind_rows(alternate_player_home_runs_markets)
  
  # Under lines
  under_lines <-
    player_home_runs_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Home Runs") |>
    select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)
  
  # Combine
  tab_player_home_runs_markets <-
    over_lines |>
    full_join(under_lines) |> 
    select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")
  
  # Fix team names
  tab_player_home_runs_markets <-
    tab_player_home_runs_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
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
    rename(player_team = team_name) |> 
    mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) |>
    relocate(player_team, opposition_team, .after = player_name) |> 
    select(-join_name, -given_name, -last_name)
  
  #===============================================================================
  # Player Hits
  #===============================================================================
  
  # Filter to player home runs markets
  player_hits_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "Hits$"))
  
  # Alternate Player Hits
  alternate_player_hits_markets <-
    player_hits_markets |> 
    mutate(market_name = if_else(str_detect(market_name, "A Hit"), "1+ Hits", market_name))
  
  # Extract player names
  player_hits_markets <-
    player_hits_markets |> 
    filter(str_detect(prop_name, "Over|Under")) |>
    mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
    mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
    mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
    mutate(line = as.numeric(line)) |>
    mutate(type = str_detect(prop_name, "Over|\\+")) |> 
    mutate(type = ifelse(type, "Over", "Under")) |> 
    mutate(line = if_else(market_name == "Alternate Player Hits", line - 0.5, line))
  
  alternate_player_hits_markets <-
    alternate_player_hits_markets |>
    mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
    mutate(line = str_extract(market_name, "\\d+")) |> 
    mutate(line = as.numeric(line) - 0.5) |> 
    transmute(match, market_name = "Player Hits", player_name, line, over_price = price, prop_id)
  
  # Over lines
  over_lines <-
    player_hits_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Hits") |>
    select(match, market_name, player_name, line, over_price = price, prop_id) |> 
    bind_rows(alternate_player_hits_markets)
  
  # Under lines
  under_lines <-
    player_hits_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Hits") |>
    select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)
  
  # Combine
  tab_player_hits_markets <-
    over_lines |>
    full_join(under_lines) |> 
    select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")
  
  # Fix team names
  tab_player_hits_markets <-
    tab_player_hits_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
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
    rename(player_team = team_name) |> 
    mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) |>
    relocate(player_team, opposition_team, .after = player_name) |> 
    select(-join_name, -given_name, -last_name)
  
  #===============================================================================
  # Write to CSV------------------------------------------------------------------
  #===============================================================================
  
  tab_player_strikeouts_markets |> write_csv("Data/scraped_odds/tab_pitcher_strikeouts.csv")
  tab_player_home_runs_markets |> write_csv("Data/scraped_odds/tab_batter_home_runs.csv")
  tab_player_hits_markets |> write_csv("Data/scraped_odds/tab_batter_hits.csv")
}

#===============================================================================
# Run safe function
#===============================================================================

safe_main_tab <- safely(main_tab)
safe_main_tab()
