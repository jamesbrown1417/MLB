# Load necessary libraries
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Odds Screen"),

  sidebarLayout(
    sidebarPanel(
      # Filters will go here
      h4("Filters"),
      textInput("match_filter", "Filter by Match"),
      textInput("market_filter", "Filter by Market"),
      textInput("player_filter", "Filter by Player"),
      checkboxInput("best_price_filter", "Only Best Price", value = FALSE),
      checkboxInput("show_both_sides_filter", "Show Markets with Over/Under", value = FALSE)
    ),

    mainPanel(
      # Odds display will go here
      h4("Odds Data"),
      dataTableOutput("odds_table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Placeholder for data loading and filtering
  # For now, let's create some dummy data
  dummy_data <- data.frame(
    Match = c("Team A vs Team B", "Team C vs Team D", "Team A vs Team B", "Team E vs Team F"),
    Market = c("Moneyline", "Spread", "Total Points", "Moneyline"),
    Player = c(NA, NA, "Player X", NA),
    Price = c(1.9, 1.85, 2.0, 2.1),
    Side = c("Over", "Under", "Over", "Under"),
    stringsAsFactors = FALSE
  )

  output$odds_table <- renderDataTable({
    # Filtering logic will be added here later
    dummy_data
  })
}

# Run the application
shinyApp(ui = ui, server = server)
