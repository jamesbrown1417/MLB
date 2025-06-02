# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# Source functions
source("Functions/fix_team_names.R")
source("Functions/normalize_player_names.R")

# Get Rosters
MLB_2025_Active_Rosters <- read_rds("Data/MLB_2025_Active_Rosters.rds") |>
  mutate(normalized_name = sapply(person_full_name, normalize_player_names, USE.NAMES = FALSE))

# Get Pitchers
pitchers <-
  MLB_2025_Active_Rosters |>
  filter(position_name == "Pitcher") |> 
  select(person_full_name, normalized_name, team_name)

# Batters
batters <-
  MLB_2025_Active_Rosters |>
  filter(position_name != "Pitcher") |> 
  select(person_full_name, normalized_name, team_name)

# URL to get responses
tab_url = "https://api.beta.tab.com.au/v1/tab-info-service/sports/Baseball/competitions/Major%20League%20Baseball?homeState=SA&jurisdiction=SA"

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
    mutate(player_name = normalize_player_names(player_name)) |>
    mutate(line = str_extract(prop_name, "\\d+")) |> 
    mutate(line = as.numeric(line) - 0.5) |> 
    transmute(match, start_time, market_name = "Pitcher Strikeouts", player_name, line, over_price = price, prop_id)
  
  # Combine
  tab_player_strikeouts_markets <-
    alternate_player_strikeouts_markets |>
    # Note: This section for TAB pitcher strikeouts only seems to process "alternate" lines (X+ SOs)
    # and there's no explicit Over/Under for a standard line (e.g. Player X O/U 4.5 K)
    # If standard O/U lines exist in `player_strikeouts_markets`, they are not processed here.
    # The `player_strikeouts_markets` df is created but not fully used for O/U.
    select(match, start_time, market_name, player_name, line, over_price, prop_id) |> 
    mutate(agency = "TAB")
  
  # Fix team names and join with roster
  tab_player_strikeouts_markets <-
    tab_player_strikeouts_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
    left_join(pitchers, by = c("player_name" = "normalized_name")) |>
    rename(player_team = team_name) |> 
    mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) |>
    relocate(player_team, opposition_team, .after = player_name) |>
    mutate(original_roster_name = person_full_name) |> # Keep original name for reference
    select(-person_full_name) # Remove to avoid confusion with normalized TAB name
  
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
    mutate(player_name = normalize_player_names(player_name)) |>
    mutate(line = str_extract(market_name, "\\d+")) |> 
    mutate(line = as.numeric(line) - 0.5) |> 
    transmute(match, start_time, market_name = "Player Home Runs", player_name, line, over_price = price, prop_id)
  
  # Over lines
  over_lines <-
    player_home_runs_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Home Runs") |>
    mutate(player_name = normalize_player_names(player_name)) |>
    select(match, start_time, market_name, player_name, line, over_price = price, prop_id) |> 
    bind_rows(alternate_player_home_runs_markets) # alternate names already normalized
  
  # Under lines
  under_lines <-
    player_home_runs_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Home Runs") |>
    mutate(player_name = normalize_player_names(player_name)) |>
    select(match, start_time, market_name, player_name, line, under_price = price, under_prop_id = prop_id)
  
  # Combine
  tab_player_home_runs_markets <-
    over_lines |>
    full_join(under_lines, by = c("match", "start_time", "market_name", "player_name", "line")) |>
    select(match, start_time, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")
  
  # Fix team names and join with roster
  tab_player_home_runs_markets <-
    tab_player_home_runs_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
    left_join(batters, by = c("player_name" = "normalized_name")) |>
    rename(player_team = team_name) |> 
    mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) |>
    relocate(player_team, opposition_team, .after = player_name) |>
    mutate(original_roster_name = person_full_name) |>
    select(-person_full_name)
  
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
    mutate(player_name = normalize_player_names(player_name)) |>
    mutate(line = str_extract(market_name, "\\d+")) |> 
    mutate(line = as.numeric(line) - 0.5) |> 
    transmute(match, start_time, market_name = "Player Hits", player_name, line, over_price = price, prop_id)
  
  # Over lines
  over_lines <-
    player_hits_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Hits") |>
    mutate(player_name = normalize_player_names(player_name)) |>
    select(match, start_time, market_name, player_name, line, over_price = price, prop_id) |> 
    bind_rows(alternate_player_hits_markets) # alternate names already normalized
  
  # Under lines
  under_lines <-
    player_hits_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Hits") |>
    mutate(player_name = normalize_player_names(player_name)) |>
    select(match, start_time, market_name, player_name, line, under_price = price, under_prop_id = prop_id)
  
  # Combine
  tab_player_hits_markets <-
    over_lines |>
    full_join(under_lines, by = c("match", "start_time", "market_name", "player_name", "line")) |>
    select(match, start_time, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")
  
  # Fix team names and join with roster
  tab_player_hits_markets <-
    tab_player_hits_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
    left_join(batters, by = c("player_name" = "normalized_name")) |>
    rename(player_team = team_name) |> 
    mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) |>
    relocate(player_team, opposition_team, .after = player_name) |> 
    relocate(start_time, .after = match) |>
    mutate(original_roster_name = person_full_name) |>
    select(-person_full_name)

  #===============================================================================
  # Player RBIs (Runs Batted In)
  #===============================================================================
  
  # Filter to player RBI markets - Adjust regex based on actual market names
  # Common names might be "Player RBIs", "Player Runs Batted In", "To Record An RBI"
  player_rbis_markets_all <-
    all_tab_markets |> 
    filter(str_detect(market_name, "RBI|Runs Batted In")) # Broad filter initially
  
  # Separate main Over/Under markets (e.g., "Player RBIs O/U 0.5")
  player_rbis_markets_main <-
    player_rbis_markets_all |> 
    filter(str_detect(market_name, "(Player RBIs|Player Runs Batted In)$")) |> # Filter for standard O/U markets
    filter(str_detect(prop_name, "Over|Under")) |>
    mutate(player_name = str_extract(prop_name, "^.*(?=\\s(Over|Under))")) |> # Extract name before Over/Under
    # mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> # Redundant if using the lookahead above
    mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
    mutate(line = as.numeric(line)) |>
    mutate(type = str_detect(prop_name, "Over|\\+")) |> 
    mutate(type = ifelse(type, "Over", "Under"))
  # No line adjustment needed here as these are standard lines (e.g., 0.5, 1.5)
  
  # Separate Alternate/Specific RBI markets (e.g., "Player To Record An RBI")
  alternate_player_rbis_markets <-
    player_rbis_markets_all |> 
    filter(str_detect(market_name, "\\+ Runs Batted In")) |> # Filter for the specific "at least one" market
    mutate(player_name = prop_name) |>
    mutate(player_name = normalize_player_names(player_name)) |>
    mutate(line = str_extract(market_name, "\\d+")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    transmute(match, start_time, market_name = "Player RBIs", player_name, line, over_price = price, prop_id)
  
  # Over lines
  over_lines_rbis <-
    player_rbis_markets_main |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player RBIs") |>
    mutate(player_name = normalize_player_names(player_name)) |>
    select(match, start_time, market_name, player_name, line, over_price = price, prop_id) |> 
    bind_rows(alternate_player_rbis_markets) # alternate names already normalized
  
  # Under lines
  under_lines_rbis <-
    player_rbis_markets_main |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player RBIs") |>
    mutate(player_name = normalize_player_names(player_name)) |>
    select(match, start_time, market_name, player_name, line, under_price = price, under_prop_id = prop_id)
  
  # Combine Over and Under lines for RBIs
  tab_player_rbis_markets <-
    over_lines_rbis |>
    full_join(under_lines_rbis, by = c("match", "start_time", "market_name", "player_name", "line")) |>
    select(match, start_time, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")
  
  # Fix team names and join with roster
  tab_player_rbis_markets <-
    tab_player_rbis_markets |> 
    filter(!is.na(match)) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE, extra = "merge") |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    left_join(batters, by = c("player_name" = "normalized_name")) |>
    rename(player_team = team_name) |> 
    mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) |>
    relocate(player_team, opposition_team, .after = player_name) |> 
    relocate(start_time, .after = match) |>
    mutate(original_roster_name = person_full_name) |>
    select(-person_full_name)
  
  #===============================================================================
  # Write to CSV------------------------------------------------------------------
  #===============================================================================
  
  tab_player_strikeouts_markets |> write_csv("Data/scraped_odds/tab_pitcher_strikeouts.csv")
  tab_player_home_runs_markets |> write_csv("Data/scraped_odds/tab_batter_home_runs.csv")
  tab_player_hits_markets |> write_csv("Data/scraped_odds/tab_batter_hits.csv")
  tab_player_rbis_markets |> write_csv("Data/scraped_odds/tab_batter_rbis.csv")
}

#===============================================================================
# Run safe function
#===============================================================================

safe_main_tab <- safely(main_tab)
safe_main_tab()
