options <- c("Please select", health_boards)

ui <- page_sidebar(
  title = "My dashboard",
  sidebar = sidebar("Sidebar",
                    card(full_screen = TRUE,
                         card_header = "Board",
                         card_body = ""),
                    selectInput("board", "Select board", options)),
  card(
    card_header("CAIR teams"),
    full_screen = TRUE,
    height = "1750px",
    ## HEADLINE FIGURES
    bslib::layout_columns(height = "500px",
                          width = 1/2, 
                          uiOutput("First_Sub"), # Date of earliest data on CAIR
                          uiOutput("Latest_Sub"), # Total submissions
                          uiOutput("CAIR_representation"),
                          uiOutput("Change_Team"),
                          uiOutput("Change_Measure")),
    card(card_header("Total submissions over time"),
         height = "1000px",
         plotOutput("Cumulative_subs"))
  ),
  card(
    card_header("Putting in the hours"),
    full_screen = TRUE,
    height = "1750px",
    ## HEADLINE FIGURES
    bslib::layout_columns(height = "500px", 
                          width = 1/2, 
                          uiOutput("Total_WTE"),
                          uiOutput("Total_BA"),
                          uiOutput("Total_EO")),
    card(card_header("Working hours over time"),
         height = "1000px",
         plotOutput("Cumulative_WTE"))
  ),
  card(
    card_header("Streaks"),
    full_screen = TRUE,
    height = "1750px",
    ## HEADLINE FIGURES
    bslib::layout_columns(max_height = "500px", 
                   uiOutput("Streak_NRV"),
                   uiOutput("Streak_rephases"),
                   uiOutput("Streak_trends"),
                   uiOutput("Streak_shifts"))
    )
)