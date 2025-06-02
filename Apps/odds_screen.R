# Load necessary libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Odds Screen"),
  
  sidebarLayout(
    sidebarPanel(
      # Filters
      h4("Filters"),
      textInput("match_filter", 
                "Filter by Match", 
                placeholder = "Enter match name..."),
      textInput("market_filter", 
                "Filter by Market", 
                placeholder = "Enter market name..."),
      textInput("player_filter", 
                "Filter by Player", 
                placeholder = "Enter player name..."),
      checkboxInput("show_both_sides_filter", 
                    "Only Show Markets with Under Price", 
                    value = FALSE),
      checkboxInput("best_over_price_filter", 
                    "Only Best Over Price", 
                    value = FALSE),
      checkboxInput("best_under_price_filter", 
                    "Only Best Under Price", 
                    value = FALSE),
      br(),
      actionButton("reset_filters", 
                   "Reset Filters", 
                   class = "btn-warning")
    ),
    
    mainPanel(
      # Summary stats
      fluidRow(
        column(4, valueBoxOutput("total_matches")),
        column(4, valueBoxOutput("total_markets")),
        column(4, valueBoxOutput("total_odds"))
      ),
      br(),
      # Odds display
      h4("Odds Data"),
      DTOutput("odds_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load data from .rds files
  loaded_data <- reactive({
    data_path <- "../Data/processed_odds/"
    
    # Check if directory exists
    if (!dir.exists(data_path)) {
      showNotification("Data directory not found!", type = "error")
      return(NULL)
    }
    
    files <- list.files(path = data_path, 
                        pattern = "\\.rds$", 
                        full.names = TRUE) %>%
      str_subset("arbs", negate = TRUE)  # Exclude arb files
    
    if (length(files) == 0) {
      showNotification("No data files found!", type = "warning")
      return(NULL)
    }
    
    # Load all files with error handling
    loaded_data_list <- files %>%
      map(function(file) {
        tryCatch({
          temp_df <- readRDS(file)
          market_category <- tools::file_path_sans_ext(basename(file))
          temp_df %>%
            mutate(Market_Category = market_category)
        }, error = function(e) {
          showNotification(paste("Error reading file:", basename(file)), 
                           type = "error")
          NULL
        })
      })
    
    # Combine all dataframes
    bind_rows(loaded_data_list)
  })
  
  # Load and process data - keep wide format
  processed_data <- reactive({
    data <- loaded_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(tibble())
    }
    
    # Ensure price columns are numeric if they exist
    if ("over_price" %in% names(data)) {
      data <- data %>% mutate(over_price = as.numeric(over_price))
    }
    
    if ("under_price" %in% names(data)) {
      data <- data %>% mutate(under_price = as.numeric(under_price))
    }
    
    # Standardize column names if needed
    if ("match" %in% names(data) && !"match_name" %in% names(data)) {
      data <- data %>% rename(match_name = match)
    }
    
    # Convert timestamps if present
    if ("start_time" %in% names(data)) {
      data <- data %>%
        mutate(start_time = as.POSIXct(start_time, 
                                       format = "%Y-%m-%d %H:%M:%S",
                                       tz = "UTC"))
    }
    
    data
  })
  
  # Reactive filtering logic
  filtered_data <- reactive({
    data <- processed_data()
    
    if (nrow(data) == 0) {
      return(data)
    }
    
    # Apply text filters
    if (!is.null(input$match_filter) && nchar(input$match_filter) > 0) {
      data <- data %>%
        filter(if ("match_name" %in% names(.)) {
          str_detect(match_name, regex(input$match_filter, ignore_case = TRUE))
        } else if ("match" %in% names(.)) {
          str_detect(match, regex(input$match_filter, ignore_case = TRUE))
        } else {
          TRUE
        })
    }
    
    if (!is.null(input$market_filter) && nchar(input$market_filter) > 0) {
      data <- data %>%
        filter(if ("market_name" %in% names(.)) {
          str_detect(market_name, regex(input$market_filter, ignore_case = TRUE))
        } else {
          TRUE
        })
    }
    
    if (!is.null(input$player_filter) && nchar(input$player_filter) > 0) {
      data <- data %>%
        filter(if ("player_name" %in% names(.)) {
          is.na(player_name) | str_detect(player_name, regex(input$player_filter, ignore_case = TRUE))
        } else {
          TRUE
        })
    }
    
    # Show markets with under price
    if (input$show_both_sides_filter) {
      if ("under_price" %in% names(data)) {
        data <- data %>%
          filter(!is.na(under_price))
      }
    }
    
    # Best over price filter
    if (input$best_over_price_filter) {
      if ("over_price" %in% names(data)) {
        grouping_vars <- c("match_name", "player_name", "market_name", "line")
        grouping_vars <- grouping_vars[grouping_vars %in% names(data)]
        
        if (length(grouping_vars) > 0) {
          data <- data %>%
            group_by(across(all_of(grouping_vars))) %>%
            filter(is.na(over_price) | over_price == max(over_price, na.rm = TRUE)) %>%
            slice_head(n = 1) %>%
            ungroup()
        }
      }
    }
    
    # Best under price filter
    if (input$best_under_price_filter) {
      if ("under_price" %in% names(data)) {
        grouping_vars <- c("match_name", "player_name", "market_name", "line")
        grouping_vars <- grouping_vars[grouping_vars %in% names(data)]
        
        if (length(grouping_vars) > 0) {
          data <- data %>%
            group_by(across(all_of(grouping_vars))) %>%
            filter(is.na(under_price) | under_price == max(under_price, na.rm = TRUE)) %>%
            slice_head(n = 1) %>%
            ungroup()
        }
      }
    }
    
    data
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateTextInput(session, "match_filter", value = "")
    updateTextInput(session, "market_filter", value = "")
    updateTextInput(session, "player_filter", value = "")
    updateCheckboxInput(session, "show_both_sides_filter", value = FALSE)
    updateCheckboxInput(session, "best_over_price_filter", value = FALSE)
    updateCheckboxInput(session, "best_under_price_filter", value = FALSE)
  })
  
  # Summary statistics
  output$total_matches <- renderValueBox({
    data <- filtered_data()
    match_col <- if ("match_name" %in% names(data)) "match_name" else "match"
    
    valueBox(
      value = if (match_col %in% names(data)) {
        n_distinct(data[[match_col]], na.rm = TRUE)
      } else {
        0
      },
      subtitle = "Total Matches",
      color = "blue"
    )
  })
  
  output$total_markets <- renderValueBox({
    data <- filtered_data()
    valueBox(
      value = if ("market_name" %in% names(data)) {
        n_distinct(data$market_name, na.rm = TRUE)
      } else {
        0
      },
      subtitle = "Total Markets",
      color = "green"
    )
  })
  
  output$total_odds <- renderValueBox({
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "Total Odds",
      color = "yellow"
    )
  })
  
  # Render data table
  output$odds_table <- renderDT({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(
        datatable(
          data.frame(Message = "No data available"),
          options = list(
            dom = 't',
            language = list(emptyTable = "No odds data found matching your filters")
          )
        )
      )
    }
    
    # Select columns to display
    display_cols <- names(data)
    
    # Columns to exclude from display
    exclude_cols <- c("home_team", "away_team", "player_team", "opposition_team", 
                      "EventKey", "MarketKey", "OutcomeKey", "original_roster_name")
    
    display_cols <- setdiff(display_cols, exclude_cols)
    
    # Reorder columns for better display
    preferred_order <- c("match_name", "match", "market_name", "player_name", 
                         "line", "over_price", "under_price", "agency", "Market_Category",
                         "start_time")
    
    display_cols <- c(
      preferred_order[preferred_order %in% display_cols],
      setdiff(display_cols, preferred_order)
    )
    
    data_display <- data %>%
      select(all_of(display_cols))
    
    # Format numeric columns
    if ("over_price" %in% names(data_display)) {
      data_display <- data_display %>%
        mutate(over_price = round(over_price, 2))
    }
    
    if ("under_price" %in% names(data_display)) {
      data_display <- data_display %>%
        mutate(under_price = round(under_price, 2))
    }
    
    if ("line" %in% names(data_display)) {
      data_display <- data_display %>%
        mutate(line = round(line, 1))
    }
    
    datatable(
      data_display,
      filter = "top",
      options = list(
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-right', targets = which(names(data_display) %in% c("over_price", "under_price", "line")) - 1)
        ),
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatCurrency(
        columns = intersect(c("over_price", "under_price"), names(data_display)),
        currency = "$",
        digits = 2
      ) %>%
      formatStyle(
        columns = if ("over_price" %in% names(data_display)) "over_price" else NULL,
        backgroundColor = styleInterval(0, c("transparent", "#e8f5e9"))
      ) %>%
      formatStyle(
        columns = if ("under_price" %in% names(data_display)) "under_price" else NULL,
        backgroundColor = styleInterval(0, c("transparent", "#fff3e0"))
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)