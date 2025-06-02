# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# Source the normalization function
source("Functions/normalize_player_names.R")

# Read in data
all_pinnacle_raw_files <- list.files("OddsScraper/Pinnacle", "\\.csv", full.names = TRUE)

all_pinnacle_data <-
  map(all_pinnacle_raw_files, read_csv) |> 
  bind_rows()

#===============================================================================
# Player Strikeouts
#===============================================================================

# Get all strikeouts Markets
strikeouts_markets <-
  all_pinnacle_data |> 
  filter(str_detect(selection, "Strikeouts"))

# Strikeouts - over
strikeouts_over <-
  strikeouts_markets |> 
  mutate(player_name = str_remove(selection, " \\(Strikeouts\\)")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  mutate(player_name = normalize_player_names(player_name)) |>
  filter(str_detect(name, "Over")) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Player Strikeouts") |>
  mutate(line = handicap) |>
  transmute(
    match,
    player_name,
    market,
    line,
    over_price = price,
    agency
  )

# Strikeouts - under
strikeouts_under <-
  strikeouts_markets |> 
  mutate(player_name = str_remove(selection, " \\(Strikeouts\\)")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  mutate(player_name = normalize_player_names(player_name)) |>
  filter(str_detect(name, "Under")) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Player Strikeouts") |>
  mutate(line = handicap) |>
  transmute(
    match,
    player_name,
    market,
    line,
    under_price = price,
    agency
  )

# Combine strikeouts data
player_strikeouts_data <-
  inner_join(strikeouts_over, strikeouts_under) |> 
  arrange(match, player_name, line, desc(over_price)) |> 
  rename(market_name = market)

# Write to CSV
write_csv(player_strikeouts_data, "Data/scraped_odds/pinnacle_strikeouts.csv")

#===============================================================================
# Player Hits
#===============================================================================

# Get all hits Markets
hits_markets <-
  all_pinnacle_data |> 
  filter(str_detect(selection, "Hits"))

# Hits - over
hits_over <-
  hits_markets |> 
  mutate(player_name = str_remove(selection, " \\(Hits\\)")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  mutate(player_name = normalize_player_names(player_name)) |>
  filter(str_detect(name, "Over")) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Player Hits") |>
  mutate(line = handicap) |>
  transmute(
    match,
    player_name,
    market,
    line,
    over_price = price,
    agency
  )

# Hits - under
hits_under <-
  hits_markets |> 
  mutate(player_name = str_remove(selection, " \\(Hits\\)")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  mutate(player_name = normalize_player_names(player_name)) |>
  filter(str_detect(name, "Under")) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Player Hits") |>
  mutate(line = handicap) |>
  transmute(
    match,
    player_name,
    market,
    line,
    under_price = price,
    agency
  )

# Combine hits data
player_hits_data <-
  inner_join(hits_over, hits_under) |> 
  arrange(match, player_name, line, desc(over_price)) |> 
  rename(market_name = market)

# Write to CSV
write_csv(player_hits_data, "Data/scraped_odds/pinnacle_hits.csv")

#===============================================================================
# Pitching Outs
#===============================================================================

# Get all pitching outs Markets
pitching_outs_markets <-
  all_pinnacle_data |> 
  filter(str_detect(selection, "Pitching Outs"))

# Pitching Outs - over
pitching_outs_over <-
  pitching_outs_markets |> 
  mutate(player_name = str_remove(selection, " \\(Pitching Outs\\)")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  mutate(player_name = normalize_player_names(player_name)) |>
  filter(str_detect(name, "Over")) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Pitching Outs") |>
  mutate(line = handicap) |>
  transmute(
    match,
    player_name,
    market,
    line,
    over_price = price,
    agency
  )

# Pitching Outs - under
pitching_outs_under <-
  pitching_outs_markets |> 
  mutate(player_name = str_remove(selection, " \\(Pitching Outs\\)")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  mutate(player_name = normalize_player_names(player_name)) |>
  filter(str_detect(name, "Under")) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Pitching Outs") |>
  mutate(line = handicap) |>
  transmute(
    match,
    player_name,
    market,
    line,
    under_price = price,
    agency
  )

# Combine pitching outs data
pitching_outs_data <-
  inner_join(pitching_outs_over, pitching_outs_under) |> 
  arrange(match, player_name, line, desc(over_price)) |> 
  rename(market_name = market)

# Write to CSV
write_csv(pitching_outs_data, "Data/scraped_odds/pinnacle_pitching_outs.csv")

#===============================================================================
# Home Runs
#===============================================================================

# Get all home run Markets
home_run_markets <-
  all_pinnacle_data |> 
  filter(str_detect(selection, "Home Run"))

# Home Run - over
home_run_over <-
  home_run_markets |> 
  mutate(player_name = str_remove(selection, " \\(Home Run\\)")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  mutate(player_name = normalize_player_names(player_name)) |>
  filter(str_detect(name, "Over")) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Home Runs") |>
  mutate(line = handicap) |>
  transmute(
    match,
    player_name,
    market,
    line,
    over_price = price,
    agency
  ) |> 
  distinct(player_name, market, line, over_price, .keep_all = TRUE)

# Home Run - under
home_run_under <-
  home_run_markets |> 
  mutate(player_name = str_remove(selection, " \\(Home Run\\)")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  mutate(player_name = normalize_player_names(player_name)) |>
  filter(str_detect(name, "Under")) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Home Runs") |>
  mutate(line = handicap) |>
  transmute(
    match,
    player_name,
    market,
    line,
    under_price = price,
    agency
  ) |> 
  distinct(player_name, market, line, under_price, .keep_all = TRUE)

# Combine home run data
home_run_data <-
  inner_join(home_run_over, home_run_under) |> 
  arrange(match, player_name, line, desc(over_price)) |> 
  rename(market_name = market)

# Write to CSV
write_csv(home_run_data, "Data/scraped_odds/pinnacle_home_runs.csv")

#===============================================================================
# Total Bases
#===============================================================================

# Get all total bases Markets
total_bases_markets <-
  all_pinnacle_data |> 
  filter(str_detect(selection, "Total Bases"))

# Total Bases - over
total_bases_over <-
  total_bases_markets |> 
  mutate(player_name = str_remove(selection, " \\(Total Bases\\)")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  mutate(player_name = normalize_player_names(player_name)) |>
  filter(str_detect(name, "Over")) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Total Bases") |>
  mutate(line = handicap) |>
  transmute(
    match,
    player_name,
    market,
    line,
    over_price = price,
    agency
  )

# Total Bases - under
total_bases_under <-
  total_bases_markets |> 
  mutate(player_name = str_remove(selection, " \\(Total Bases\\)")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  mutate(player_name = normalize_player_names(player_name)) |>
  filter(str_detect(name, "Under")) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Total Bases") |>
  mutate(line = handicap) |>
  transmute(
    match,
    player_name,
    market,
    line,
    under_price = price,
    agency
  )

# Combine total bases data
total_bases_data <-
  inner_join(total_bases_over, total_bases_under) |> 
  arrange(match, player_name, line, desc(over_price)) |> 
  rename(market_name = market)

# Write to CSV
write_csv(total_bases_data, "Data/scraped_odds/pinnacle_total_bases.csv")

#===============================================================================
# Final Output Summary
#===============================================================================

cat("Data processing complete. The following CSVs have been created:\n",
    "- pinnacle_strikeouts.csv\n",
    "- pinnacle_hits.csv\n",
    "- pinnacle_pitching_outs.csv\n",
    "- pinnacle_home_runs.csv\n",
    "- pinnacle_total_bases.csv\n")