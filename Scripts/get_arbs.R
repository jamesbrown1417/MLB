# library(shiny)
library(bslib)
library(gridlayout)
library(DT)
library(tidyverse)

#===============================================================================
# Read in Data
#===============================================================================

# Fix team names function
source("Scripts/fix_team_names.R")

##%######################################################%##
#                                                          #
####                    Head to Head                    ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_odds_files <-
  list.files("Data/scraped_odds", full.names = TRUE, pattern = "h2h") |>
  map(read_csv) |>
  # Keep if nrow of dataframe greater than 0
  keep(~nrow(.x) > 0) |>
  reduce(bind_rows)

# Get Start Times
start_times <-
  all_odds_files |>
  filter(agency == "TAB") |> 
  select(match, start_time) |>
  distinct(match, .keep_all = TRUE)

# For each match, get all home wins
all_home <-
  all_odds_files |>
  arrange(match, start_time, desc(home_win)) |>
  select(match, start_time, market_name, home_team, home_win, home_agency = agency) |> 
  mutate(start_time = date(start_time)) |> 
  select(-start_time)

# For each match, get all away wins
all_away <-
  all_odds_files |>
  arrange(match, start_time, desc(away_win)) |>
  select(match, start_time, market_name, away_team, away_win, away_agency = agency) |> 
  mutate(start_time = date(start_time)) |> 
  select(-start_time)

# Combine
all_odds_h2h <-
  all_home |>
  full_join(all_away, relationship = "many-to-many", by = c("match", "market_name")) |>
  mutate(margin = (1/home_win + 1/away_win)) |> 
  mutate(margin = round(100*(margin - 1), digits = 3)) |> 
  arrange(margin)

##%######################################################%##
#                                                          #
####                   Batter Hits                      ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_hits <-
  list.files("Data/scraped_odds",
             full.names = TRUE,
             pattern = "hits") |>
  map(read_csv) |>
  # Ignore null elements
  keep( ~ nrow(.x) > 0) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price)) |>
  select(-matches("id")) |> 
  mutate(market_name = "Batter Hits")

##%######################################################%##
#                                                          #
####               Pitcher Strikeouts                   ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_strikeouts <-
  list.files("Data/scraped_odds",
             full.names = TRUE,
             pattern = "strikeouts") |>
  map(read_csv) |>
  # Ignore null elements
  keep( ~ nrow(.x) > 0) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price)) |>
  select(-matches("id")) |> 
  mutate(market_name = "Pitcher Strikeouts")

##%######################################################%##
#                                                          #
####                    Batter RBIs                     ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_rbis <-
  list.files("Data/scraped_odds",
             full.names = TRUE,
             pattern = "rbis|runs_batted_in") |>
  map(read_csv) |>
  # Ignore null elements
  keep( ~ nrow(.x) > 0) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price)) |>
  select(-matches("id")) |>
  mutate(market_name = "Batter RBIs")

##%######################################################%##
#                                                          #
####                    Batter Home Runs                ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_hrs <-
  list.files("Data/scraped_odds",
             full.names = TRUE,
             pattern = "hr|home_run") |>
  map(read_csv) |>
  # Ignore null elements
  keep( ~ nrow(.x) > 0) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price)) |>
  select(-matches("id")) |>
  mutate(market_name = "Batter Home Runs")


##%######################################################%##
#                                                          #
####   Get all over under comparisons of same market    ####
#                                                          #
##%######################################################%##

# H2H---------------------------------------------------------------------------
h2h_arbs <-
  all_odds_h2h |> 
  mutate(margin = -1*margin) |> 
  filter(margin > 0)

# Hits---------------------------------------------------------------------------
# Check if all_player_hits has data before processing
if (nrow(all_player_hits) > 0) {
  hits_unders <-
    all_player_hits |>
    # Ensure market_name column exists or is added if not already
    # mutate(market_name = if("market_name" %in% names(.)) market_name else "Batter Hits") |>
    filter(market_name == "Batter Hits") |>
    select(
      match,
      market_name,
      player_name,
      line,
      under_price,
      agency
    ) |>
    filter(!is.na(under_price)) |>
    rename(under_agency = agency)

  hits_overs <-
    all_player_hits |>
    # mutate(market_name = if("market_name" %in% names(.)) market_name else "Batter Hits") |>
    filter(market_name == "Batter Hits") |>
    select(
      match,
      market_name,
      player_name,
      line,
      over_price,
      agency
    ) |>
    filter(!is.na(over_price)) |> # Ensure over_price is not NA
    rename(over_agency = agency)

  hits_arbs <-
    hits_unders |>
    inner_join(
      hits_overs,
      by = c(
        "match",
        "market_name",
        "player_name",
        "line"
      ),
      relationship = "many-to-many"
    ) |>
    # Ensure we don't join same agency for over and under
    filter(under_agency != over_agency) |> 
    relocate(under_price, .after = over_price) |>
    mutate(margin_calc = 1 / under_price + 1 / over_price) |> # This is the bookmaker's combined margin
    arrange(margin_calc) |> # Lower is better for bookmaker, but we want 1 - this value
    mutate(margin = (1 - margin_calc)) |> # This is the arbitrage potential. Positive means arb.
    mutate(margin = 100 * margin) |> # Convert to percentage
    # filter(margin > 0) |> # Uncomment to keep only arbitrage opportunities
    distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |>
    arrange(desc(margin)) # Arrange by highest arb margin
} else {
  hits_arbs <- tibble() # Empty tibble if no hits data
  warning("No player hits data to process for arbitrage.")
}


# Strikeouts---------------------------------------------------------------------
# Check if all_player_strikeouts has data
if (nrow(all_player_strikeouts) > 0) {
  strikeouts_unders <-
    all_player_strikeouts |>
    # mutate(market_name = if("market_name" %in% names(.)) market_name else "Pitcher Strikeouts") |>
    filter(market_name == "Pitcher Strikeouts") |>
    select(
      match,
      market_name,
      player_name,
      line,
      under_price,
      agency
    ) |>
    filter(!is.na(under_price)) |>
    rename(under_agency = agency)

  strikeouts_overs <-
    all_player_strikeouts |>
    # mutate(market_name = if("market_name" %in% names(.)) market_name else "Pitcher Strikeouts") |>
    filter(market_name == "Pitcher Strikeouts") |>
    select(
      match,
      market_name,
      player_name,
      line,
      over_price,
      agency
    ) |>
    filter(!is.na(over_price)) |>
    rename(over_agency = agency)

  strikeouts_arbs <-
    strikeouts_unders |>
    inner_join(
      strikeouts_overs,
      by = c(
        "match",
        "market_name",
        "player_name",
        "line"
      ),
      relationship = "many-to-many"
    ) |>
    filter(under_agency != over_agency) |>
    relocate(under_price, .after = over_price) |>
    mutate(margin_calc = 1 / under_price + 1 / over_price) |>
    arrange(margin_calc) |>
    mutate(margin = (1 - margin_calc)) |> # Positive margin indicates arbitrage
    mutate(margin = 100 * margin) |>
    # filter(margin > 0) |> # Uncomment to keep only arbitrage opportunities
    distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |>
    arrange(desc(margin))
} else {
  strikeouts_arbs <- tibble()
  warning("No player strikeouts data to process for arbitrage.")
}

# RBIs---------------------------------------------------------------------------
# Check if all_player_rbis has data
if (nrow(all_player_rbis) > 0) {
  rbis_unders <-
    all_player_rbis |>
    # mutate(market_name = if("market_name" %in% names(.)) market_name else "Batter RBIs") |>
    filter(market_name == "Batter RBIs") |>
    select(
      match,
      market_name,
      player_name,
      line,
      under_price,
      agency
    ) |>
    filter(!is.na(under_price)) |>
    rename(under_agency = agency)

  rbis_overs <-
    all_player_rbis |>
    # mutate(market_name = if("market_name" %in% names(.)) market_name else "Batter RBIs") |>
    filter(market_name == "Batter RBIs") |>
    select(
      match,
      market_name,
      player_name,
      line,
      over_price,
      agency
    ) |>
    filter(!is.na(over_price)) |>
    rename(over_agency = agency)

  rbis_arbs <-
    rbis_unders |>
    inner_join(
      rbis_overs,
      by = c(
        "match",
        "market_name",
        "player_name",
        "line"
      ),
      relationship = "many-to-many"
    ) |>
    filter(under_agency != over_agency) |>
    relocate(under_price, .after = over_price) |>
    mutate(margin_calc = 1 / under_price + 1 / over_price) |>
    arrange(margin_calc) |>
    mutate(margin = (1 - margin_calc)) |> # Positive margin indicates arbitrage
    mutate(margin = 100 * margin) |>
    # filter(margin > 0) |> # Uncomment to keep only arbitrage opportunities
    distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |>
    arrange(desc(margin))
} else {
  rbis_arbs <- tibble()
  warning("No player RBIs data to process for arbitrage.")
}

# Home Runs---------------------------------------------------------------------

# Check if all_player_hrs has data
if (nrow(all_player_hrs) > 0) {
  hrs_unders <-
    all_player_hrs |>
    # mutate(market_name = if("market_name" %in% names(.)) market_name else "Batter Home Runs") |>
    filter(market_name == "Batter Home Runs") |>
    select(
      match,
      market_name,
      player_name,
      line,
      under_price,
      agency
    ) |>
    filter(!is.na(under_price)) |>
    rename(under_agency = agency)

  hrs_overs <-
    all_player_hrs |>
    # mutate(market_name = if("market_name" %in% names(.)) market_name else "Batter Home Runs") |>
    filter(market_name == "Batter Home Runs") |>
    select(
      match,
      market_name,
      player_name,
      line,
      over_price,
      agency
    ) |>
    filter(!is.na(over_price)) |>
    rename(over_agency = agency)

  hrs_arbs <-
    hrs_unders |>
    inner_join(
      hrs_overs,
      by = c(
        "match",
        "market_name",
        "player_name",
        "line"
      ),
      relationship = "many-to-many"
    ) |>
    filter(under_agency != over_agency) |>
    relocate(under_price, .after = over_price) |>
    mutate(margin_calc = 1 / under_price + 1 / over_price) |>
    arrange(margin_calc) |>
    mutate(margin = (1 - margin_calc)) |> # Positive margin indicates arbitrage
    mutate(margin = 100 * margin) |>
    # filter(margin > 0) |> # Uncomment to keep only arbitrage opportunities
    distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |>
    arrange(desc(margin))
} else {
  hrs_arbs <- tibble()
  warning("No player home runs data to process for arbitrage.")
}


#===============================================================================
# Write out
#===============================================================================

all_player_hits |>
  filter(!is.na(player_name)) |> 
  write_rds("Data/processed_odds/all_player_hits.rds")

all_player_strikeouts |>
  filter(!is.na(player_name)) |> 
  write_rds("Data/processed_odds/all_player_strikeouts.rds")

all_player_rbis |>
  filter(!is.na(player_name)) |> 
  write_rds("Data/processed_odds/all_player_rbis.rds")

all_player_hrs |>
  filter(!is.na(player_name)) |> 
  write_rds("Data/processed_odds/all_player_hrs.rds")

hits_arbs |>
  bind_rows(strikeouts_arbs) |>
  bind_rows(rbis_arbs) |> 
  bind_rows(hrs_arbs) |>
  filter(!is.na(player_name)) |> 
  write_rds("Data/processed_odds/all_player_arbs.rds")

  