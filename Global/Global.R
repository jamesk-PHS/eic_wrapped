
library(tidyverse)
library(shiny)
library(bslib)
library(eicmethods)
library(odbc)
library(shinyjs)



channel <- dbConnect(odbc(),
                     dsn="DVPROD",
                     uid = keyring::key_list(keyring = "DATABASE")[1,2],
                     pwd = keyring::key_get(keyring = "DATABASE",
                                            service = "DVPROD"))


useShinyjs()

ui <- source("ui.R")


##### more soon


server <- source("server.R")$value


shinyApp(ui, server)
