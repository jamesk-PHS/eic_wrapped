source("Server/Health_Board_Server/Summary_Server.R")
source("Server/Streaks_Server/Streaks_Server.R")
source("Server/HB_Hours_Server/HB_Hours_Server.R")


server <- function(input, output, session) {
  
  
  HB_Server(input, output, session)
  Streaks_Server(input, output, session)
  
  HB_Hours_Server(input, output, session)
}

server
