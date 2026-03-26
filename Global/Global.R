
library(tidyverse)

library(shiny)
library(bslib)
library(odbc)
library(shinyjs)

library(eicmethods)
library(phsstyles)
library(scales)
library(bsicons)



channel <- dbConnect(odbc(),
                     dsn="DVPROD",
                     uid = keyring::key_list(keyring = "DATABASE")[1,2],
                     pwd = keyring::key_get(keyring = "DATABASE",
                                            service = "DVPROD"))






ui <- source("ui.R")$value


##### more soon


server <- source("server.R")$value


shinyApp(ui, server)
