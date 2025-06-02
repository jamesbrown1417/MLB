# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# Fix team names function
source("Functions/fix_team_names.R") # Corrected path
# Source player name normalization function
source("Functions/normalize_player_names.R")

# Read scraped HTML from the BET365_HTML Folder
scraped_files_player <- list.files("OddsScraper/Bet365/Data/", full.names = TRUE, pattern = "player")

# Main Function
get_player_props <- function(scraped_file) {

  # Get Markets
  bet365_player_markets <-
    read_html(scraped_file) |>
    html_nodes(".gl-MarketGroupPod")
  
    # Extract match details
    match_teams <- read_html(scraped_file) |>
      html_elements(".sph-FixturePodHeader_TeamName ") |>
      html_text() |> 
      str_trim()
    
    # Format the match string as "HomeTeam v AwayTeam"
    match_name <- glue("{match_teams[2]} v {match_teams[1]}")

  # Market Names
  market_names <-
    bet365_player_markets |>
    html_elements(".cm-MarketGroupWithIconsButton_Text, .sc-MarketGroupButtonWithStats_Text") |>
    html_text()
  
  #=============================================================================
  # Pitcher Strikeouts Over / Under
  #=============================================================================
  
  strikeouts_over_under_index <- which(market_names == "Pitcher Strikeouts O/U")
  
  strikeouts_players <-
    bet365_player_markets[[strikeouts_over_under_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text() |>
    sapply(normalize_player_names)
  
  strikeouts_cols <-
    bet365_player_markets[[strikeouts_over_under_index]] |>
    html_elements(".gl-Market_General")
  
  strikeouts_over_index <- which(str_detect(strikeouts_cols |> html_text(), "Over"))
  
  strikeouts_over_lines <-
    strikeouts_cols[[strikeouts_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  strikeouts_over_odds <-
    strikeouts_cols[[strikeouts_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  strikeouts_under_index <- which(str_detect(strikeouts_cols |> html_text(), "Under"))
  
  strikeouts_under_odds <-
    strikeouts_cols[[strikeouts_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  pitcher_strikeouts_ou <-
    tibble(player = strikeouts_players,
           line = as.numeric(strikeouts_over_lines),
           over_price = as.numeric(strikeouts_over_odds),
           under_price = as.numeric(strikeouts_under_odds)) |>
    mutate(market_name = "Pitcher Strikeouts Over/Under") |>
    mutate(agency = "Bet365")
  
  #=============================================================================
  # Alternate Pitcher Strikeouts
  #=============================================================================
  
  alternate_strikeouts_index <- which(market_names == "Pitcher Strikeouts")
  
  alternate_strikeouts_players <-
    bet365_player_markets[[alternate_strikeouts_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text() |>
    sapply(normalize_player_names)
  
  alternate_strikeouts_cols <-
    bet365_player_markets[[alternate_strikeouts_index]] |>
    html_elements(".gl-Market_General")
  
  alt_so_lines <- c(3:8)
  alt_so_tables <- list()
  for(l in alt_so_lines) {
    idx <- which(str_detect(alternate_strikeouts_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), paste0("^", l, "$")))
    alt_odds <- alternate_strikeouts_cols[[idx]] |>
      html_elements(".gl-ParticipantOddsOnly_Odds") |>
      html_text()
    alt_so_tables[[as.character(l)]] <-
      tibble(player = alternate_strikeouts_players,
             line = l,
             over_price = as.numeric(alt_odds)) |>
      mutate(market_name = "Alternate Pitcher Strikeouts") |>
      mutate(agency = "Bet365")
  }
  
  alternate_pitcher_strikeouts <-
    bind_rows(alt_so_tables) |>
    filter(!is.na(over_price))
  
  team_names <-
    scraped_file |> 
    read_html() |> 
    html_nodes(".sph-FixturePodHeader_TeamName ") |> 
    html_text()
  
  team_names <- fix_team_names(team_names)
  match_name <- paste(team_names, collapse = " v ")
  
  pitcher_strikeouts_all <-
    bind_rows(pitcher_strikeouts_ou, alternate_pitcher_strikeouts) |>
    arrange(player, line, over_price) |>
    mutate(market_name = "Pitcher Strikeouts") |>
    mutate(match = match_name) |>
    relocate(match, .before = player)
  
  #=============================================================================
  # Total Bases Over / Under
  #=============================================================================
  
  total_bases_ou_index <- which(market_names == "Total Bases O/U")
  
  total_bases_players <-
    bet365_player_markets[[total_bases_ou_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text() |>
    sapply(normalize_player_names)
  
  total_bases_cols <-
    bet365_player_markets[[total_bases_ou_index]] |>
    html_elements(".gl-Market_General")
  
  total_bases_over_index <- which(str_detect(total_bases_cols |> html_text(), "Over"))
  
  total_bases_over_lines <-
    total_bases_cols[[total_bases_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  total_bases_over_odds <-
    total_bases_cols[[total_bases_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  total_bases_under_index <- which(str_detect(total_bases_cols |> html_text(), "Under"))
  
  total_bases_under_odds <-
    total_bases_cols[[total_bases_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  total_bases_ou <-
    tibble(player = total_bases_players,
           line = as.numeric(total_bases_over_lines),
           over_price = as.numeric(total_bases_over_odds),
           under_price = as.numeric(total_bases_under_odds)) |>
    mutate(market_name = "Total Bases Over/Under") |>
    mutate(agency = "Bet365")
  
  #=============================================================================
  # Alternate Total Bases
  #=============================================================================
  
  alternate_total_bases_index <- which(market_names == "Total Bases")
  
  alternate_total_bases_players <-
    bet365_player_markets[[alternate_total_bases_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text() |>
    sapply(normalize_player_names)
  
  alternate_total_bases_cols <-
    bet365_player_markets[[alternate_total_bases_index]] |>
    html_elements(".gl-Market_General")
  
  alt_tb_lines <- 1:4
  alt_tb_tables <- list()
  for(l in alt_tb_lines) {
    idx <- which(str_detect(alternate_total_bases_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), paste0("^", l, "$")))
    alt_odds <- alternate_total_bases_cols[[idx]] |>
      html_elements(".gl-ParticipantOddsOnly_Odds") |>
      html_text()
    alt_tb_tables[[as.character(l)]] <-
      tibble(player = alternate_total_bases_players,
             line = l,
             over_price = as.numeric(alt_odds)) |>
      mutate(market_name = "Alternate Total Bases") |>
      mutate(agency = "Bet365")
  }
  
  alternate_total_bases <-
    bind_rows(alt_tb_tables) |>
    filter(!is.na(over_price))
  
  total_bases_all <-
    bind_rows(total_bases_ou, alternate_total_bases) |>
    arrange(player, line, over_price) |>
    mutate(market_name = "Total Bases") |>
    mutate(match = match_name) |>
    relocate(match, .before = player)
  
  #=============================================================================
  # Hits Over / Under
  #=============================================================================
  
  hits_ou_index <- which(market_names == "Hits O/U")
  
  hits_players <-
    bet365_player_markets[[hits_ou_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text() |>
    sapply(normalize_player_names)
  
  hits_cols <-
    bet365_player_markets[[hits_ou_index]] |>
    html_elements(".gl-Market_General")
  
  hits_over_index <- which(str_detect(hits_cols |> html_text(), "Over"))
  
  hits_over_lines <-
    hits_cols[[hits_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  hits_over_odds <-
    hits_cols[[hits_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  hits_under_index <- which(str_detect(hits_cols |> html_text(), "Under"))
  
  hits_under_odds <-
    hits_cols[[hits_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  hits_ou <-
    tibble(player = hits_players,
           line = as.numeric(hits_over_lines),
           over_price = as.numeric(hits_over_odds),
           under_price = as.numeric(hits_under_odds)) |>
    mutate(market_name = "Hits Over/Under") |>
    mutate(agency = "Bet365")
  
  #=============================================================================
  # Alternate Hits
  #=============================================================================
  
  alternate_hits_index <- which(market_names == "Hits")
  
  alternate_hits_players <-
    bet365_player_markets[[alternate_hits_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text() |>
    sapply(normalize_player_names)
  
  alternate_hits_cols <-
    bet365_player_markets[[alternate_hits_index]] |>
    html_elements(".gl-Market_General")
  
  alt_hits_lines <- 1:4
  alt_hits_tables <- list()
  for(l in alt_hits_lines) {
    idx <- which(str_detect(alternate_hits_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), paste0("^", l, "$")))
    alt_odds <- alternate_hits_cols[[idx]] |>
      html_elements(".gl-ParticipantOddsOnly_Odds") |>
      html_text()
    alt_hits_tables[[as.character(l)]] <-
      tibble(player = alternate_hits_players,
             line = l,
             over_price = as.numeric(alt_odds)) |>
      mutate(market_name = "Alternate Hits") |>
      mutate(agency = "Bet365")
  }
  
  alternate_hits <-
    bind_rows(alt_hits_tables) |>
    filter(!is.na(over_price))
  
  hits_all <-
    bind_rows(hits_ou, alternate_hits) |>
    arrange(player, line, over_price) |>
    mutate(market_name = "Hits") |>
    mutate(match = match_name) |>
    relocate(match, .before = player)
  
  #=============================================================================
  # Runs Over / Under
  #=============================================================================
  
  runs_ou_index <- which(market_names == "Runs O/U")
  
  runs_players <-
    bet365_player_markets[[runs_ou_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text() |>
    sapply(normalize_player_names)
  
  runs_cols <-
    bet365_player_markets[[runs_ou_index]] |>
    html_elements(".gl-Market_General")
  
  runs_over_index <- which(str_detect(runs_cols |> html_text(), "Over"))
  
  runs_over_lines <-
    runs_cols[[runs_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  runs_over_odds <-
    runs_cols[[runs_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  runs_under_index <- which(str_detect(runs_cols |> html_text(), "Under"))
  
  runs_under_odds <-
    runs_cols[[runs_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  runs_ou <-
    tibble(player = runs_players,
           line = as.numeric(runs_over_lines),
           over_price = as.numeric(runs_over_odds),
           under_price = as.numeric(runs_under_odds)) |>
    mutate(market_name = "Runs Over/Under") |>
    mutate(agency = "Bet365")
  
  #=============================================================================
  # Alternate Runs
  #=============================================================================
  
  alternate_runs_index <- which(market_names == "Runs")
  
  alternate_runs_players <-
    bet365_player_markets[[alternate_runs_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text() |>
    sapply(normalize_player_names)
  
  alternate_runs_cols <-
    bet365_player_markets[[alternate_runs_index]] |>
    html_elements(".gl-Market_General")
  
  alt_runs_lines <- 1:4
  alt_runs_tables <- list()
  for(l in alt_runs_lines) {
    idx <- which(str_detect(alternate_runs_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), paste0("^", l, "$")))
    alt_odds <- alternate_runs_cols[[idx]] |>
      html_elements(".gl-ParticipantOddsOnly_Odds") |>
      html_text()
    alt_runs_tables[[as.character(l)]] <-
      tibble(player = alternate_runs_players,
             line = l,
             over_price = as.numeric(alt_odds)) |>
      mutate(market_name = "Alternate Runs") |>
      mutate(agency = "Bet365")
  }
  
  alternate_runs <-
    bind_rows(alt_runs_tables) |>
    filter(!is.na(over_price))
  
  runs_all <-
    bind_rows(runs_ou, alternate_runs) |>
    arrange(player, line, over_price) |>
    mutate(market_name = "Runs") |>
    mutate(match = match_name) |>
    relocate(match, .before = player)
  
  #=============================================================================
  # Runs Batted In Over / Under
  #=============================================================================
  
  rbi_ou_index <- which(market_names == "Runs Batted In O/U")
  
  rbi_players <-
    bet365_player_markets[[rbi_ou_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text() |>
    sapply(normalize_player_names)
  
  rbi_cols <-
    bet365_player_markets[[rbi_ou_index]] |>
    html_elements(".gl-Market_General")
  
  rbi_over_index <- which(str_detect(rbi_cols |> html_text(), "Over"))
  
  rbi_over_lines <-
    rbi_cols[[rbi_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  rbi_over_odds <-
    rbi_cols[[rbi_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  rbi_under_index <- which(str_detect(rbi_cols |> html_text(), "Under"))
  
  rbi_under_odds <-
    rbi_cols[[rbi_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  rbi_ou <-
    tibble(player = rbi_players,
           line = as.numeric(rbi_over_lines),
           over_price = as.numeric(rbi_over_odds),
           under_price = as.numeric(rbi_under_odds)) |>
    mutate(market_name = "Runs Batted In Over/Under") |>
    mutate(agency = "Bet365")
  
  #=============================================================================
  # Alternate Runs Batted In
  #=============================================================================
  
  alternate_rbi_index <- which(market_names == "Runs Batted In")
  
  alternate_rbi_players <-
    bet365_player_markets[[alternate_rbi_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text() |>
    sapply(normalize_player_names)
  
  alternate_rbi_cols <-
    bet365_player_markets[[alternate_rbi_index]] |>
    html_elements(".gl-Market_General")
  
  alt_rbi_lines <- 1:4
  alt_rbi_tables <- list()
  for(l in alt_rbi_lines) {
    idx <- which(str_detect(alternate_rbi_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), paste0("^", l, "$")))
    alt_odds <- alternate_rbi_cols[[idx]] |>
      html_elements(".gl-ParticipantOddsOnly_Odds") |>
      html_text()
    alt_rbi_tables[[as.character(l)]] <-
      tibble(player = alternate_rbi_players,
             line = l,
             over_price = as.numeric(alt_odds)) |>
      mutate(market_name = "Alternate Runs Batted In") |>
      mutate(agency = "Bet365")
  }
  
  alternate_rbi <-
    bind_rows(alt_rbi_tables) |>
    filter(!is.na(over_price))
  
  rbi_all <-
    bind_rows(rbi_ou, alternate_rbi) |>
    arrange(player, line, over_price) |>
    mutate(market_name = "Runs Batted In") |>
    mutate(match = match_name) |>
    relocate(match, .before = player)
  
  #===============================================================================
  # Combine markets together
  #===============================================================================
  
  return(
    pitcher_strikeouts_all |>
      bind_rows(total_bases_all) |>
      bind_rows(hits_all) |>
      bind_rows(runs_all) |>
      bind_rows(rbi_all) |>
    arrange(player, line, over_price) |> 
    relocate(under_price, .after = over_price) |> 
    filter(!is.na(over_price) | !is.na(under_price)) |>
    mutate(match = match_name) |> 
    relocate(match, .before = player) |> 
    rename(player_name = player)
  )
}

get_player_props_safe <- safely(get_player_props)

list_of_player_props <- map(scraped_files_player, get_player_props_safe)

list_of_player_props <-
  list_of_player_props |>
  keep(~is.null(.x$error)) |>
  map_dfr("result")

all_player_props <-
  list_of_player_props |>
  separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(line = if_else(line %% 1 == 0, line - 0.5, line))

pitcher_strikeouts <- all_player_props |> filter(market_name == "Pitcher Strikeouts")
total_bases <- all_player_props |> filter(market_name == "Total Bases")
hits <- all_player_props |> filter(market_name == "Hits")
runs <- all_player_props |> filter(market_name == "Runs")
rbis <- all_player_props |> filter(market_name == "Runs Batted In")

write_csv(pitcher_strikeouts, "Data/scraped_odds/bet365_pitcher_strikeouts.csv")
write_csv(total_bases, "Data/scraped_odds/bet365_total_bases.csv")
write_csv(hits, "Data/scraped_odds/bet365_hits.csv")
write_csv(runs, "Data/scraped_odds/bet365_runs.csv")
write_csv(rbis, "Data/scraped_odds/bet365_runs_batted_in.csv")