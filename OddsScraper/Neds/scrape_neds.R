# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# Source helper functions
source("Functions/fix_team_names.R")
source("Functions/normalize_player_names.R")

# Get Rosters
MLB_2025_Active_Rosters <- read_rds("Data/MLB_2025_Active_Rosters.rds") |>
  mutate(normalized_name = sapply(person_full_name, normalize_player_names))

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
  select(match, start_time, market_name, home_team, away_team, line, over_price, under_price, agency)

##%######################################################%##
#                                                          #
####                  Match Line Data                   ####
#                                                          #
##%######################################################%##

# Filter to only include line markets
line_data <-
  market_df |> 
  filter(str_detect(market_name, "^Line")) |> 
  select(-market_name)

# Home lines
line_home <-
  line_data |> 
  separate(match_name, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match_name = paste(home_team, "v", away_team)) |>
  mutate(line = str_extract(entrants, "\\(.*\\)")) |>
  mutate(line = str_remove(line, "\\(")) |>
  mutate(line = str_remove(line, "\\)")) |>
  mutate(line = as.numeric(line)) |>
  mutate(team = str_remove(entrants, " \\(.*\\)")) |>
  mutate(team = fix_team_names(team)) |> 
  filter(team == home_team) |>
  select(match = match_name, start_time, home_team, line, home_price = price)

# Away lines
line_away <-
  line_data |> 
  separate(match_name, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match_name = paste(home_team, "v", away_team)) |>
  mutate(line = str_extract(entrants, "\\(.*\\)")) |>
  mutate(line = str_remove(line, "\\(")) |>
  mutate(line = str_remove(line, "\\)")) |>
  mutate(line = as.numeric(line)) |>
  mutate(team = str_remove(entrants, " \\(.*\\)")) |>
  mutate(team = fix_team_names(team)) |> 
  filter(team == away_team) |>
  select(match = match_name, start_time, away_team, line, away_price = price)

# Merge home and away lines
lines_all <-
  line_home |> 
  left_join(line_away) |> 
  mutate(margin = round(1 / home_price + 1 / away_price, digits = 2)) |>
  mutate(agency = "Neds") |>
  mutate(market_name = "Line") |> 
  select(match, start_time, market_name, home_team, away_team, line, home_price, away_price, margin, agency)

##%######################################################%##
#                                                          #
####                 Pitcher Strikeouts                 ####
#                                                          #
##%######################################################%##

# Filter to only include pitcher strikeouts markets
pitcher_strikeouts <-
  market_df |> 
  filter(str_detect(market_name, "Strikeouts"))

pitcher_strikeouts_alternate <-
  pitcher_strikeouts |> 
  filter(str_detect(entrants, "\\+")) |>
  mutate(line = str_extract(entrants, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  mutate(player_name = str_remove(entrants, " \\d+.*$")) |>
  select(match = match_name, start_time, player_name, line, over_price = price)

pitcher_strikeouts_over <- 
  pitcher_strikeouts |> 
  filter(str_detect(entrants, "Over")) |>
  mutate(line = as.numeric(handicaps)) |> 
  mutate(player_name = str_remove(market_name, "^Pitcher Strikeouts O/U - ")) |>
  select(match = match_name, start_time, player_name, line, over_price = price)

pitcher_strikeouts_under <-
  pitcher_strikeouts |> 
  filter(str_detect(entrants, "Under")) |>
  mutate(line = as.numeric(handicaps)) |> 
  mutate(player_name = str_remove(market_name, "^Pitcher Strikeouts O/U - ")) |>
  select(match = match_name, start_time, player_name, line, under_price = price)

pitcher_strikeouts_all <-
  pitcher_strikeouts_alternate |> 
  bind_rows(pitcher_strikeouts_over) |>
  left_join(pitcher_strikeouts_under) |>
  separate(match, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(player_name = normalize_player_names(player_name)) |>
  left_join(pitchers, by = c("player_name" = "normalized_name")) |>
  mutate(opposition_team = if_else(team_name == home_team, away_team, home_team)) |>
  rename(player_team = team_name) |>
  relocate(player_team, opposition_team, .after = player_name) |> 
  select(-person_full_name) |> # person_full_name is from the join, keep the original player_name
  mutate(market_name = "Pitcher Strikeouts") |> 
  mutate(agency = "Neds")

##%######################################################%##
#                                                          #
####                   Pitcher Outs                     ####
#                                                          #
##%######################################################%##

# Filter to only include pitcher outs markets
pitcher_outs <-
  market_df |> 
  filter(str_detect(market_name, "Player Pitcher Outs O/U|Outs Recorded"))

if (nrow(pitcher_outs) == 0) {
  pitcher_outs <- tibble(
    match_name = character(),
    home_team = character(),
    away_team = character(),
    start_time = character(),
    market_name = character(),
    entrants = character(),
    handicaps = numeric(),
    price = numeric()
  )
}

pitcher_outs_over <- 
  pitcher_outs |> 
  filter(str_detect(entrants, "Over")) |>
  mutate(line = as.numeric(str_extract(entrants, "\\d+\\.\\d+"))) |>
  mutate(player_name = str_remove(market_name, "^Player Pitcher Outs O/U - |^Outs Recorded O/U - ")) |>
  mutate(player_name = str_remove(player_name, " \\(\\d+\\.?\\d*\\)$")) |>
  select(match = match_name, start_time, player_name, line, over_price = price)

pitcher_outs_under <-
  pitcher_outs |> 
  filter(str_detect(entrants, "Under")) |>
  mutate(line = as.numeric(str_extract(entrants, "\\d+\\.\\d+"))) |>
  mutate(player_name = str_remove(market_name, "^Player Pitcher Outs O/U - |^Outs Recorded O/U - ")) |>
  mutate(player_name = str_remove(player_name, " \\(\\d+\\.?\\d*\\)$")) |>
  select(match = match_name, start_time, player_name, line, under_price = price)

pitcher_outs_all <-
  pitcher_outs_over |>
  left_join(pitcher_outs_under) |>
  separate(match, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(player_name = normalize_player_names(player_name)) |>
  left_join(pitchers, by = c("player_name" = "normalized_name")) |>
  mutate(opposition_team = if_else(team_name == home_team, away_team, home_team)) |>
  rename(player_team = team_name) |>
  relocate(player_team, opposition_team, .after = player_name) |> 
  select(-person_full_name) |>
  mutate(market_name = "Pitcher Outs") |> 
  mutate(agency = "Neds")

if (nrow(pitcher_outs_all) != 0) {
  pitcher_outs_all <-
    pitcher_outs_all |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team))
}

##%######################################################%##
#                                                          #
####                Pitcher Hits Allowed                ####
#                                                          #
##%######################################################%##

# Filter to only include pitcher hits allowed markets
pitcher_hits_allowed <-
  market_df |> 
  filter(str_detect(market_name, "Hits Allowed"))

pitcher_hits_allowed_over <- 
  pitcher_hits_allowed |> 
  filter(str_detect(entrants, "Over")) |>
  mutate(line = as.numeric(str_extract(entrants, "\\d+\\.\\d+"))) |>
  mutate(player_name = str_remove(market_name, "Player Pitcher Hits Allowed O/U - ")) |>
  mutate(player_name = str_remove(player_name, " \\(\\d+\\.?\\d*\\)$")) |>
  select(match = match_name, start_time, player_name, line, over_price = price)

pitcher_hits_allowed_under <-
  pitcher_hits_allowed |> 
  filter(str_detect(entrants, "Under")) |>
  mutate(line = as.numeric(str_extract(entrants, "\\d+\\.\\d+"))) |>
  mutate(player_name = str_remove(market_name, "Player Pitcher Hits Allowed O/U - ")) |>
  mutate(player_name = str_remove(player_name, " \\(\\d+\\.?\\d*\\)$")) |>
  select(match = match_name, start_time, player_name, line, under_price = price)

pitcher_hits_allowed_all <-
  pitcher_hits_allowed_over |>
  left_join(pitcher_hits_allowed_under) |>
  separate(match, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(player_name = normalize_player_names(player_name)) |>
  left_join(pitchers, by = c("player_name" = "normalized_name")) |>
  mutate(opposition_team = if_else(team_name == home_team, away_team, home_team)) |>
  rename(player_team = team_name) |>
  relocate(player_team, opposition_team, .after = player_name) |> 
  select(-person_full_name) |>
  mutate(market_name = "Pitcher Hits Allowed") |> 
  mutate(agency = "Neds")

if (nrow(pitcher_hits_allowed_all) != 0) {
  pitcher_hits_allowed_all <-
    pitcher_hits_allowed_all |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team))
}

##%######################################################%##
#                                                         #
####                  Batter Hits                      ####
#                                                         #
##%######################################################%##

batter_hits_markets <-
  market_df |>
  filter(str_detect(market_name, regex("To Have .* Hits", ignore_case = TRUE)) &
           !str_detect(market_name, regex("Team|Game|Strikeouts|Pitcher|No Hit", ignore_case = TRUE)))

batter_hits_alternate <-
  batter_hits_markets |>
  mutate(line_str = str_extract(market_name, "\\d+(?=\\s*\\+)")) |>
  filter(!is.na(line_str)) |>
  mutate(line = as.numeric(line_str) - 0.5) |>
  mutate(player_name = normalize_player_names(str_trim(str_remove(entrants, " \\(.*\\)$")))) |>
  mutate(player_team = str_extract(entrants, "\\(([^)]+)\\)")) |>
  mutate(player_team = str_remove_all(player_team, "\\(|\\)")) |>
  select(match = match_name, start_time, player_name, player_team, line, over_price = price)

batter_hits_all <-
  batter_hits_alternate |>
  # Join with batters data to verify player and get official team (if needed for consistency)
  # For now, we are trusting player_team from Neds if available
  # left_join(batters, by = c("player_name" = "normalized_name")) |>
  separate(match, c("home_team", "away_team"), sep = " vs ", remove = FALSE, fill = "right") |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |> # Normalize team name from Neds
  mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
  relocate(player_team, opposition_team, .after = player_name) |>
  # We don't have a join with batters by default here, so no batter specific columns like person_full_name to remove unless added
  mutate(market_name = "Batter Hits") |>
  mutate(agency = "Neds")

##%######################################################%##
#                                                         #
####                 Batter Home Runs                  ####
#                                                         #
##%######################################################%##

batter_hr_markets <-
  market_df |>
  filter(str_detect(market_name, regex("To Have .* Home Run|To Hit .* Home Run", ignore_case = TRUE)) &
           !str_detect(market_name, regex("Team|Game", ignore_case = TRUE))) |> 
  mutate(market_name = str_replace(market_name, "To Hit a", "To Hit 1+"))

batter_hr_alternate <-
  batter_hr_markets |>
  mutate(line_str = str_extract(market_name, "\\d+(?=\\s*\\+)")) |>
  filter(!is.na(line_str)) |>
  mutate(line = as.numeric(line_str) - 0.5) |>
  mutate(player_name = normalize_player_names(str_trim(str_remove(entrants, " \\(.*\\)$")))) |>
  mutate(player_team = str_extract(entrants, "\\(([^)]+)\\)")) |>
  mutate(player_team = str_remove_all(player_team, "\\(|\\)")) |>
  select(match = match_name, start_time, player_name, player_team, line, over_price = price)

batter_hr_all <-
  batter_hr_alternate |>
  separate(match, c("home_team", "away_team"), sep = " vs ", remove = FALSE, fill = "right") |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
  relocate(player_team, opposition_team, .after = player_name) |>
  mutate(market_name = "Batter Home Runs") |>
  mutate(agency = "Neds")

##%######################################################%##
#                                                         #
####                  Batter RBIs                      ####
#                                                         #
##%######################################################%##

batter_rbi_markets <-
  market_df |>
  filter(str_detect(market_name, regex("To Have .* RBI|To Record .* RBI|Runs Batted In", ignore_case = TRUE)) &
           !str_detect(market_name, regex("Team|Game", ignore_case = TRUE)))

batter_rbi_alternate <-
  batter_rbi_markets |>
  mutate(line_str = str_extract(market_name, "\\d+(?=\\s*\\+)")) |>
  filter(!is.na(line_str)) |>
  mutate(line = as.numeric(line_str) - 0.5) |>
  mutate(player_name = normalize_player_names(str_trim(str_remove(entrants, " \\(.*\\)$")))) |>
  mutate(player_team = str_extract(entrants, "\\(([^)]+)\\)")) |>
  mutate(player_team = str_remove_all(player_team, "\\(|\\)")) |>
  select(match = match_name, start_time, player_name, player_team, line, over_price = price)

batter_rbi_all <-
  batter_rbi_alternate |>
  separate(match, c("home_team", "away_team"), sep = " vs ", remove = FALSE, fill = "right") |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
  relocate(player_team, opposition_team, .after = player_name) |>
  mutate(market_name = "Batter RBIs") |>
  mutate(agency = "Neds")

##%######################################################%##
#                                                         #
####                Batter Stolen Bases                ####
#                                                         #
##%######################################################%##

batter_sb_markets <-
  market_df |>
  filter(str_detect(market_name, regex("To Have .* Stolen Base|To Record .* Stolen Base|To Steal", ignore_case = TRUE)) &
           !str_detect(market_name, regex("Team|Game", ignore_case = TRUE)))

batter_sb_alternate <-
  batter_sb_markets |>
  mutate(line_str = str_extract(market_name, "\\d+(?=\\s*\\+)")) |>
  filter(!is.na(line_str)) |>
  mutate(line = as.numeric(line_str) - 0.5) |>
  mutate(player_name = normalize_player_names(str_trim(str_remove(entrants, " \\(.*\\)$")))) |>
  mutate(player_team = str_extract(entrants, "\\(([^)]+)\\)")) |>
  mutate(player_team = str_remove_all(player_team, "\\(|\\)")) |>
  select(match = match_name, start_time, player_name, player_team, line, over_price = price)

batter_sb_all <-
  batter_sb_alternate |>
  separate(match, c("home_team", "away_team"), sep = " vs ", remove = FALSE, fill = "right") |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
  relocate(player_team, opposition_team, .after = player_name) |>
  mutate(market_name = "Batter Stolen Bases") |>
  mutate(agency = "Neds")
  
##%######################################################%##
#                                                          #
####                  Write out as CSV                  ####
#                                                          #
##%######################################################%##

h2h_data |> write_csv("Data/scraped_odds/neds_h2h.csv")
totals_all |> write_csv("Data/scraped_odds/neds_total_runs.csv")
lines_all |> write_csv("Data/scraped_odds/neds_match_line.csv")
pitcher_strikeouts_all |> write_csv("Data/scraped_odds/neds_pitcher_strikeouts.csv")
pitcher_outs_all |> write_csv("Data/scraped_odds/neds_pitcher_outs.csv")
pitcher_hits_allowed_all |> write_csv("Data/scraped_odds/neds_pitcher_hits_allowed.csv")
batter_hits_all |> write_csv("Data/scraped_odds/neds_batter_hits.csv")
batter_hr_all |> write_csv("Data/scraped_odds/neds_batter_home_runs.csv")
batter_rbi_all |> write_csv("Data/scraped_odds/neds_batter_rbis.csv")
batter_sb_all |> write_csv("Data/scraped_odds/neds_batter_stolen_bases.csv")
