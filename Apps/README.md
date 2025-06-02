# Odds Screen Shiny App

This Shiny app provides a screen to display and filter odds data.

## Features

- Filter odds by Match
- Filter odds by Market
- Filter odds by Player
- Option to display only the best price for a given market
- Option to display only markets where both under and over odds are present

## How to Run

1.  **Ensure R and Shiny are installed.** If not, you can install R from [CRAN](https://cran.r-project.org/) and then install Shiny by running `install.packages("shiny")` in your R console.
2.  **Navigate to the `Apps` directory** in your R environment.
3.  **Run the app** using the following command in your R console:
    ```R
    shiny::runApp("odds_screen.R")
    ```
