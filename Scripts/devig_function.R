library(tidyverse)

# Function to convert decimal odds to implied probabilities
odds_to_prob <- function(odds) {
  return(1 / odds)
}

# Function to de-vig using the Shin method
de_vig_shin <- function(over_price, under_price) {
  # Convert odds to implied probabilities
  p_over <- odds_to_prob(over_price)
  p_under <- odds_to_prob(under_price)
  
  # Calculate total implied probability with vig
  total_prob <- p_over + p_under
  
  # Estimate Shin's margin parameter (lambda)
  lambda <- (p_over - p_under) / (1 - total_prob)
  
  # Adjust implied probabilities
  true_prob_over <- (p_over - lambda) / (1 - 2 * lambda)
  true_prob_under <- (p_under - lambda) / (1 - 2 * lambda)
  
  # Normalize probabilities to sum to 1
  sum_prob <- true_prob_over + true_prob_under
  true_prob_over <- true_prob_over / sum_prob
  true_prob_under <- true_prob_under / sum_prob
  
  return(tibble(over_price_devigged = 1 / true_prob_over, under_price_devigged = 1 / true_prob_under))
}

# Function to de-vig using the Logarithmic method
de_vig_log <- function(over_price, under_price) {
  # Convert odds to implied probabilities
  p_over <- odds_to_prob(over_price)
  p_under <- odds_to_prob(under_price)
  
  # Calculate the vig-adjusted probabilities
  total_prob <- p_over + p_under
  margin <- total_prob - 1
  
  adj_prob_over <- p_over / total_prob
  adj_prob_under <- p_under / total_prob
  
  return(tibble(over_price_devigged = 1 / adj_prob_over, under_price_devigged = 1 / adj_prob_under))
}

# Main function to de-vig a line using selected method
de_vig_line <- function(over_price, under_price, method = "shin") {
  if (over_price <= 1 || under_price <= 1) {
    stop("Odds must be greater than 1")
  }
  
  if (method == "shin") {
    return(de_vig_shin(over_price, under_price))
  } else if (method == "log") {
    return(de_vig_log(over_price, under_price))
  } else {
    stop("Invalid method. Choose 'shin' or 'log'.")
  }
}
