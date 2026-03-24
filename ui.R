useShinyjs()

map(list.files("UI", recursive = TRUE, full.names = TRUE), source)

page_navbar(
  title = "Your year in CAIR",
  navbar_options = navbar_options(
    bg = "#0062cc",
    underline = TRUE
  ),
  nav_menu(
    title = "Menu",
    align = "right",
    nav_panel("Home", Home_Page),
    nav_panel("Summary", Summary),
    nav_panel("Streaks", Streaks),
    nav_panel("Hours", HB_Hours)
  )
)