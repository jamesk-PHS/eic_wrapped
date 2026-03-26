boards <- c("Please select", unique(eicmethods::raw_submission_data()$health_board_name))

HB_Hours <- page_sidebar(
  title = "My dashboard",
  sidebar = sidebar("Sidebar",
                    card(full_screen = TRUE,
                         card_header = "Putting in the hours",
                         card_body = ""),
                    selectInput("board", "Select board", boards)),
  card(
    full_screen = TRUE,
    ## HEADLINE FIGURES
    layout_columns(max_height = "250px", 
                   uiOutput("Total_WTE"),
                   uiOutput("Total_BA"),
                   uiOutput("Total_EO")
    ),
    ## MAIN PLOTS
    uiOutput("WTE_hours")
  )
)