library(tidyverse)

# Create function to fix MLB Team Names
fix_team_names <- function(team_vector) {
  new_vector <- case_when(
    str_detect(team_vector, "(Arizona)|(Diamondbacks)|(D[\\- ]?backs)") ~ "Arizona Diamondbacks",
    str_detect(team_vector, "(Atlanta)|(Braves)") ~ "Atlanta Braves",
    str_detect(team_vector, "(Baltimore)|(Orioles)") ~ "Baltimore Orioles",
    str_detect(team_vector, "(Boston)|(Red Sox)") ~ "Boston Red Sox",
    str_detect(team_vector, "(Chicago Cubs)|(Cubs)") ~ "Chicago Cubs",
    str_detect(team_vector, "(Chicago White Sox)|(White Sox)") ~ "Chicago White Sox",
    str_detect(team_vector, "(Cincinnati)|(Reds)") ~ "Cincinnati Reds",
    str_detect(team_vector, "(Cleveland)|(Guardians)|(Indians)") ~ "Cleveland Guardians",
    str_detect(team_vector, "(Colorado)|(Rockies)") ~ "Colorado Rockies",
    str_detect(team_vector, "(Detroit)|(Tigers)") ~ "Detroit Tigers",
    str_detect(team_vector, "(Houston)|(Astros)") ~ "Houston Astros",
    str_detect(team_vector, "(Kansas City)|(Royals)") ~ "Kansas City Royals",
    str_detect(team_vector, "(Los Angeles Angels)|(LA Angels)|(Angels)") ~ "Los Angeles Angels",
    str_detect(team_vector, "(Los Angeles Dodgers)|(LA Dodgers)|(Dodgers)") ~ "Los Angeles Dodgers",
    str_detect(team_vector, "(Miami)|(Marlins)") ~ "Miami Marlins",
    str_detect(team_vector, "(Milwaukee)|(Brewers)") ~ "Milwaukee Brewers",
    str_detect(team_vector, "(Minnesota)|(Twins)") ~ "Minnesota Twins",
    str_detect(team_vector, "(New York Mets)|(NY Mets)|(Mets)") ~ "New York Mets",
    str_detect(team_vector, "(New York Yankees)|(NY Yankees)|(Yankees)") ~ "New York Yankees",
    str_detect(team_vector, "(Oakland)|(Athletics)|(A's)") ~ "Oakland Athletics",
    str_detect(team_vector, "(Philadelphia)|(Phillies)") ~ "Philadelphia Phillies",
    str_detect(team_vector, "(Pittsburgh)|(Pirates)") ~ "Pittsburgh Pirates",
    str_detect(team_vector, "(San Diego)|(Padres)") ~ "San Diego Padres",
    str_detect(team_vector, "(San Francisco)|(Giants)") ~ "San Francisco Giants",
    str_detect(team_vector, "(Seattle)|(Mariners)") ~ "Seattle Mariners",
    str_detect(team_vector, "(St\\.? Louis)|(Cardinals)") ~ "St. Louis Cardinals",
    str_detect(team_vector, "(Tampa Bay)|(Rays)") ~ "Tampa Bay Rays",
    str_detect(team_vector, "(Texas)|(Rangers)") ~ "Texas Rangers",
    str_detect(team_vector, "(Toronto)|(Blue Jays)") ~ "Toronto Blue Jays",
    str_detect(team_vector, "(Washington)|(Nationals)|(Nats)") ~ "Washington Nationals",
    TRUE ~ ""  # Default case where the team name is not recognized
  )
  return(new_vector)
}