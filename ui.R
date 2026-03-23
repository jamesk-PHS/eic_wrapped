

map(list.files("UI", recursive = TRUE, full.names = TRUE), source)

nav_panel(value = "maintabid",
          title = div(tags$a(img(src="", width=120, alt = ""),
                             href= "",
                             target = "_blank"),
                      style = "position: relative; top: -10px;"),
          windowTitle = "CAIR Wrapped", #title for browser tab
          header = tags$head(includeCSS("www/phs_style.css"), # CSS styles
                             HTML("<html lang='en'>")),
          ##### Tab Panels
          #Home_Page,
          #Summary
          )