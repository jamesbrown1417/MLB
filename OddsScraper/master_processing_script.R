#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

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

