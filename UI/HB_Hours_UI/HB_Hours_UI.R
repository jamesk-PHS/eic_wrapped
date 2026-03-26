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
                   value_box(
                     title = "Total WTE",
                     value = textOutput("total_WTE"),
                     theme = phsstyles::phs_colors()[1],
                     p("The total baseline hours your team worked")
                   ),
                   value_box(
                     title = "Total B&A",
                     value = textOutput("total_SSUBA"),
                     theme = phsstyles::phs_colors()[2],
                     p("The total bank and agency hours your team had")
                   ),
                   value_box(
                     title = "Total E&O",
                     value = textOutput("total_SSUEO"),
                     theme = phsstyles::phs_colors()[4],
                     p("The total excess and overtime hours your team worked")
                   )
    ),
    ## MAIN PLOTS
    uiOutput("WTE_hours")
  )
)