Streaks <- card(
  page_sidebar(
    title = "Streaks",
    sidebar = sidebar(
      title = "Submission data",
      selectInput("board_sub_data", "Select a HB", sub_board_options)),
    card(
      uiOutput("submission_streak_plot")
    )
  ),
  page_sidebar(
    title = "Streaks",
    sidebar = sidebar(
      title = "Measure data",
      selectInput("board_NRV_data", "Select a HB", NRV_board_options)),
    card(
      uiOutput("NRV_streak_plot")
    )
  )
)