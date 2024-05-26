#===============================================================================
# Libraries
#===============================================================================

library(baseballr)
library(tidyverse)

#===============================================================================
# Get Team Rosters
#===============================================================================

# Get all team info
all_teams <-
  mlb_teams(season = 2024) |> 
  filter(sport_name == "Major League Baseball")

# Create function that takes team ID and team name and returns the roster
get_team_roster <- function(team_id, team_name) {
  # Get the team roster
  team_roster <-
    mlb_rosters(team_id, season = 2024, roster_type = "active") |> 
    mutate(team_name = team_name) |> 
    relocate(team_name, .after = person_full_name)
  
  return(team_roster)
}

# Map over each team to get all rosters
all_rosters <-
all_teams |> 
  select(team_id, team_name = team_full_name) |>
  pmap(get_team_roster, .progress = TRUE) |>
  bind_rows()


#===============================================================================
# Write out as RDS
#===============================================================================

write_rds(all_rosters, "Data/MLB_2024_Active_Rosters.rds")