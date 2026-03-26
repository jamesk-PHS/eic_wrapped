Summary <- page_sidebar(
  title = "Health Board data",
  sidebar = sidebar(
    selectInput("board", "Select board", boards)
  ),
  card(
    full_screen = TRUE,
    card_header("Your year in EiC"),
    layout_columns(max_height = "250px",
                   uiOutput("team_difference"),
                   uiOutput("measure_difference")))
)
