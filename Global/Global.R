
## Set up ---- 

library(tidyverse)

library(shiny)
library(bslib)
library(shinyjs)

library(eicmethods)
library(phsstyles)
library(scales)
library(bsicons)


eicmethods::connect_to_denodo()

health_boards <- dplyr::collect(dplyr::tbl(dv_con, dplyr::sql("SELECT DISTINCT health_board_name FROM eic.eic_all_data ORDER BY health_board_name"))) |> 
  pull()

ui <- source("UI/HB_page.R")$value

server <- source("Server/HB_server.R")$value

shinyApp(ui, server)
