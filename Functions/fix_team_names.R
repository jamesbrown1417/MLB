library(dplyr)

fix_team_names <- function(team_names) {
  team_names = case_when(
    grepl("Yankees", team_names, ignore.case = TRUE) ~ "New York Yankees",
    grepl("Red Sox|Boston", team_names, ignore.case = TRUE) ~ "Boston Red Sox",
    grepl("Blue Jays|Toronto", team_names, ignore.case = TRUE) ~ "Toronto Blue Jays",
    grepl("Orioles|Baltimore", team_names, ignore.case = TRUE) ~ "Baltimore Orioles",
    grepl("Rays|Tampa Bay", team_names, ignore.case = TRUE) ~ "Tampa Bay Rays",
    grepl("White Sox|WSox", team_names, ignore.case = TRUE) ~ "Chicago White Sox",
    grepl("Indians|Guardians|Cleveland", team_names, ignore.case = TRUE) ~ "Cleveland Guardians",
    grepl("Tigers|Detroit", team_names, ignore.case = TRUE) ~ "Detroit Tigers",
    grepl("Royals|Kansas City", team_names, ignore.case = TRUE) ~ "Kansas City Royals",
    grepl("Twins|Minnesota", team_names, ignore.case = TRUE) ~ "Minnesota Twins",
    grepl("Astros|Houston", team_names, ignore.case = TRUE) ~ "Houston Astros",
    grepl("Athletics|A's|Oakland", team_names, ignore.case = TRUE) ~ "Oakland Athletics",
    grepl("Mariners|Seattle", team_names, ignore.case = TRUE) ~ "Seattle Mariners",
    grepl("Rangers|Texas", team_names, ignore.case = TRUE) ~ "Texas Rangers",
    grepl("Braves|Atlanta", team_names, ignore.case = TRUE) ~ "Atlanta Braves",
    grepl("Marlins|Miami", team_names, ignore.case = TRUE) ~ "Miami Marlins",
    grepl("Mets", team_names, ignore.case = TRUE) ~ "New York Mets",
    grepl("Phillies|Philadelphia", team_names, ignore.case = TRUE) ~ "Philadelphia Phillies",
    grepl("Nationals|Washington", team_names, ignore.case = TRUE) ~ "Washington Nationals",
    grepl("Cubs", team_names, ignore.case = TRUE) ~ "Chicago Cubs",
    grepl("Reds|Cincinnati", team_names, ignore.case = TRUE) ~ "Cincinnati Reds",
    grepl("Brewers|Milwaukee", team_names, ignore.case = TRUE) ~ "Milwaukee Brewers",
    grepl("Pirates|Pittsburgh", team_names, ignore.case = TRUE) ~ "Pittsburgh Pirates",
    grepl("Cardinals|St Louis", team_names, ignore.case = TRUE) ~ "St. Louis Cardinals",
    grepl("Diamondbacks|D-backs|Arizona", team_names, ignore.case = TRUE) ~ "Arizona Diamondbacks",
    grepl("Rockies|Colorado", team_names, ignore.case = TRUE) ~ "Colorado Rockies",
    grepl("Dodgers", team_names, ignore.case = TRUE) ~ "Los Angeles Dodgers",
    grepl("Padres|San Diego", team_names, ignore.case = TRUE) ~ "San Diego Padres",
    grepl("Giants|San Francisco", team_names, ignore.case = TRUE) ~ "San Francisco Giants",
    grepl("Angels", team_names, ignore.case = TRUE) ~ "Los Angeles Angels",
    TRUE ~ NA # Default case where the team_names name is not recognized
  )
  return(team_names)
}
