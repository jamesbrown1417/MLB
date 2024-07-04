#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

# # Run all odds scraping scripts-----------------------------------------------
run_scraping <- function(script_name) {
  tryCatch({
    source(script_name)
  }, error = function(e) {
    cat("Odds not released yet for:", script_name, "\n")
  })
}

# Run all odds scraping scripts
run_scraping("OddsScraper/scrape_betr.R")
run_scraping("OddsScraper/scrape_BetRight.R")
run_scraping("OddsScraper/scrape_pointsbet.R")
run_scraping("OddsScraper/scrape_sportsbet.R")
run_scraping("OddsScraper/scrape_TAB.R")
# run_scraping("OddsScraper/scrape_TopSport.R")
# run_scraping("OddsScraper/scrape_bet365.R")
# run_scraping("OddsScraper/scrape_bluebet.R")
run_scraping("OddsScraper/Neds/scrape_neds.R")
# run_scraping("OddsScraper/scrape_unibet.R")
# run_scraping("OddsScraper/scrape_dabble.R")

#===============================================================================
# H2H
#===============================================================================

# Read in the data
all_h2h_files <- list.files("Data/scraped_odds", "h2h", full.names = TRUE)

all_h2h_data <-
  all_h2h_files |> 
  map_dfr(read_csv)

# Get the best home odds
best_home_odds <-
  all_h2h_data |> 
  group_by(match, home_team, away_team) |> 
  arrange(desc(home_win)) |> 
  slice_head(n = 1) |> 
  select(-away_win, -margin) |>
  rename(home_agency = agency) |> 
  ungroup()

# Get the best away odds
best_away_odds <-
  all_h2h_data |> 
  group_by(match, home_team, away_team) |> 
  arrange(desc(away_win)) |> 
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
# Totals
#===============================================================================

# Read in the data
all_totals_files <- list.files("Data/scraped_odds", "total", full.names = TRUE)

all_totals_data <-
  all_totals_files |> 
  map_dfr(read_csv)

# Get the best over odds
best_over_odds <-
  all_totals_data |> 
  group_by(match, home_team, away_team, line) |> 
  arrange(desc(over_price)) |> 
  slice_head(n = 1) |> 
  select(-under_price, -margin) |>
  rename(over_agency = agency) |> 
  ungroup()

# Get the best under odds
best_under_odds <-
  all_totals_data |> 
  group_by(match, home_team, away_team, line) |> 
  arrange(desc(under_price)) |> 
  slice_head(n = 1) |> 
  select(-over_price, -margin) |>
  rename(under_agency = agency) |> 
  ungroup()

# Merge the data
best_totals <-
  best_over_odds |> 
  inner_join(best_under_odds, by = c("match", "home_team", "away_team", "market_name", "line", "start_time")) |> 
  mutate(margin = (1/over_price + 1/under_price)) |>
  mutate(margin = 100 - 100*margin) |> 
  arrange(desc(margin))
