#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)
source("Functions/normalize_player_names.R") # Source the function

# # Run all odds scraping scripts-----------------------------------------------
run_scraping <- function(script_name) {
  tryCatch({
    source(script_name)
  }, error = function(e) {
    cat("Odds not released yet for:", script_name, "\n")
  })
}

# Run all odds scraping scripts
# run_scraping("OddsScraper/scrape_betr.R")
# run_scraping("OddsScraper/scrape_BetRight.R")
# run_scraping("OddsScraper/scrape_pointsbet.R")
# run_scraping("OddsScraper/scrape_sportsbet.R")
# run_scraping("OddsScraper/scrape_TAB.R")
# run_scraping("OddsScraper/scrape_bet365.R")
# run_scraping("OddsScraper/Neds/scrape_neds.R")
# run_scraping("OddsScraper/scrape_unibet.R")
# run_scraping("OddsScraper/scrape_dabble.R")

#===============================================================================
# H2H
#===============================================================================

# Read in the data
all_h2h_files <- list.files("Data/scraped_odds", "h2h", full.names = TRUE)

all_h2h_data <-
  all_h2h_files |> 
  map_dfr(read_csv) |> 
  arrange(match, start_time, desc(home_win))

# Get the best home odds
best_home_odds <-
  all_h2h_data |> 
  group_by(match, start_time, home_team, away_team) |> 
  arrange(match, start_time, desc(home_win)) |> 
  slice_head(n = 1) |> 
  select(-away_win, -margin) |>
  rename(home_agency = agency) |> 
  ungroup()

# Get the best away odds
best_away_odds <-
  all_h2h_data |> 
  group_by(match, start_time, home_team, away_team) |> 
  arrange(match, start_time, desc(away_win)) |> 
  slice_head(n = 1) |> 
  select(-home_win, -margin) |>
  rename(away_agency = agency) |> 
  ungroup()

# Merge the data
best_odds <-
  best_home_odds |> 
  inner_join(best_away_odds, by = c("match", "home_team", "away_team", "market_name", "start_time")) |> 
  mutate(margin = (1/home_win + 1/away_win)) |>
  mutate(margin = 100 - 100*margin) |> 
  arrange(desc(margin))

#===============================================================================
# Match Lines
#===============================================================================

# Read in the data
all_lines_files <- list.files("Data/scraped_odds", "match_line", full.names = TRUE)

all_lines_data <-
  all_lines_files |> 
  map_dfr(read_csv) |> 
  arrange(match, start_time, home_team, away_team, line, desc(home_price))

# Get the best home odds
best_home_line_odds <-
  all_lines_data |> 
  group_by(match, start_time, home_team, away_team, line) |> 
  arrange(match, start_time, home_team, away_team, line, desc(home_price)) |> 
  slice_head(n = 1) |> 
  select(-away_price) |>
  rename(home_agency = agency) |> 
  ungroup()

# Get the best away odds
best_away_line_odds <-
  all_lines_data |> 
  group_by(match, start_time, home_team, away_team, line) |> 
  arrange(match, start_time, home_team, away_team, line, desc(away_price)) |> 
  slice_head(n = 1) |> 
  select(-home_price) |>
  rename(away_agency = agency) |> 
  ungroup()

# Merge the data
best_line_odds <-
  best_home_line_odds |> 
  inner_join(best_away_line_odds) |> 
  mutate(margin = (1/home_price + 1/away_price)) |>
  mutate(margin = 100 - 100*margin) |> 
  arrange(desc(margin))

#===============================================================================
# Totals
#===============================================================================

# Read in the data
all_totals_files <- list.files("Data/scraped_odds", "total", full.names = TRUE)

all_totals_data <-
  all_totals_files |> 
  map_dfr(read_csv) |> 
  arrange(match, start_time, home_team, away_team, line, desc(over_price))

# Get the best over odds
best_over_odds <-
  all_totals_data |> 
  group_by(match, start_time, home_team, away_team, line) |> 
  arrange(match, start_time, home_team, away_team, line, desc(over_price)) |> 
  slice_head(n = 1) |> 
  select(-under_price) |>
  rename(over_agency = agency) |> 
  ungroup()

# Get the best under odds
best_under_odds <-
  all_totals_data |> 
  group_by(match, start_time, home_team, away_team, line) |> 
  arrange(match, start_time, home_team, away_team, line, desc(under_price)) |> 
  slice_head(n = 1) |> 
  select(-over_price) |>
  rename(under_agency = agency) |> 
  ungroup()

# Merge the data
best_totals <-
  best_over_odds |> 
  inner_join(best_under_odds) |> 
  mutate(margin = (1/over_price + 1/under_price)) |>
  mutate(margin = 100 - 100*margin) |> 
  arrange(desc(margin))

#===============================================================================
# Pitcher Strikeouts
#===============================================================================

# Read in the data
all_pitcher_strikeouts_files <- list.files("Data/scraped_odds", "pitcher_strikeouts", full.names = TRUE)

all_pitcher_strikeouts_data <-
  all_pitcher_strikeouts_files |> 
  map_dfr(read_csv) |> 
  mutate(player_name = sapply(player_name, normalize_player_names, USE.NAMES = FALSE)) |>
  arrange(match, home_team, away_team, player_name, line, desc(over_price))

# Get the best over odds
best_over_strikeouts_odds <-
  all_pitcher_strikeouts_data |> 
  group_by(match, home_team, away_team, player_name, line) |> 
  arrange(match, home_team, away_team, player_name, line, desc(over_price)) |> 
  slice_head(n = 1) |> 
  select(-under_price) |>
  select(-contains(("_id"))) |> 
  rename(over_agency = agency) |> 
  ungroup() |> 
  select(-start_time)

# Get the best under odds
best_under_strikeouts_odds <-
  all_pitcher_strikeouts_data |> 
  filter(!is.na(under_price)) |> 
  group_by(match, home_team, away_team, player_name, line) |> 
  arrange(match, home_team, away_team, player_name, line, desc(under_price)) |> 
  slice_head(n = 1) |> 
  select(-over_price) |>
  select(-contains(("_id"))) |> 
  rename(under_agency = agency) |> 
  ungroup() |> 
  select(-start_time)

# Merge the data
best_pitcher_strikeouts <-
  best_over_strikeouts_odds |> 
  inner_join(best_under_strikeouts_odds) |> 
  mutate(margin = (1/over_price + 1/under_price)) |>
  mutate(margin = 100 - 100*margin) |> 
  arrange(desc(margin))

#===============================================================================
# Batter Hits
#===============================================================================

# Read in the data
all_batter_hits_files <- list.files("Data/scraped_odds", "hits", full.names = TRUE)

all_batter_hits_data <-
  all_batter_hits_files |>
  map(read_csv) |>
  map_dfr(~ .x |>
            mutate(
              across(
                any_of(c("line", "over_price", "under_price")),
                as.numeric
              )
            )) |> 
  map_dfr(~ .x |>
            mutate(
              across(
                any_of(c("start_time")),
                as.datetime,
              )
            )) |>
  mutate(player_name = sapply(player_name, normalize_player_names, USE.NAMES = FALSE)) |>
  arrange(match, home_team, away_team, player_name, line, desc(over_price)) |> 
  mutate(market_name = "Batter Hits")

# Get the best over odds
best_over_batter_hits_odds <-
  all_batter_hits_data |>
  group_by(match, home_team, away_team, player_name, line) |>
  arrange(match, home_team, away_team, player_name, line, desc(over_price)) |>
  slice_head(n = 1) |>
  select(-under_price) |>
  select(-contains(("_id"))) |>
  rename(over_agency = agency) |>
  ungroup() |>
  select(-start_time)

# Get the best under odds
best_under_batter_hits_odds <-
  all_batter_hits_data |>
  filter(!is.na(under_price)) |>
  group_by(match, home_team, away_team, player_name, line) |>
  arrange(match, home_team, away_team, player_name, line, desc(under_price)) |>
  slice_head(n = 1) |>
  select(-over_price) |>
  select(-contains(("_id"))) |>
  rename(under_agency = agency) |>
  ungroup() |>
  select(-start_time)

# Merge the data
best_batter_hits <-
  best_over_batter_hits_odds |>
  left_join(best_under_batter_hits_odds) |>
  mutate(margin = (1/over_price + 1/under_price)) |>
  mutate(margin = 100 - 100 * margin) |>
  arrange(desc(margin))

#===============================================================================
# Batter RBIs
#===============================================================================

# Read in the data
all_batter_rbis_files <- list.files("Data/scraped_odds", "rbis|runs_batted_in", full.names = TRUE)

all_batter_rbis_data <-
  all_batter_rbis_files |>
  map_dfr(read_csv) |>
  mutate(player_name = sapply(player_name, normalize_player_names, USE.NAMES = FALSE)) |>
  arrange(match, home_team, away_team, player_name, line, desc(over_price)) |> 
  mutate(market_name = "Batter RBIs")

# Get the best over odds
best_over_batter_rbis_odds <-
  all_batter_rbis_data |>
  group_by(match, home_team, away_team, player_name, line) |>
  arrange(match, home_team, away_team, player_name, line, desc(over_price)) |>
  slice_head(n = 1) |>
  select(-under_price) |>
  select(-contains("_id")) |>
  rename(over_agency = agency) |>
  ungroup() |>
  select(-start_time)

# Get the best under odds
best_under_batter_rbis_odds <-
  all_batter_rbis_data |>
  filter(!is.na(under_price)) |>
  group_by(match, home_team, away_team, player_name, line) |>
  arrange(match, home_team, away_team, player_name, line, desc(under_price)) |>
  slice_head(n = 1) |>
  select(-over_price) |>
  select(-contains("_id")) |>
  rename(under_agency = agency) |>
  ungroup() |>
  select(-start_time)

# Merge the data
best_batter_rbis <-
  best_over_batter_rbis_odds |>
  inner_join(best_under_batter_rbis_odds) |>
  mutate(margin = (1/over_price + 1/under_price)) |>
  mutate(margin = 100 - 100 * margin) |>
  arrange(desc(margin))
