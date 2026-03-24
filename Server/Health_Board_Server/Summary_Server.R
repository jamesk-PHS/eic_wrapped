HB_Server <- function(input, output, session){
  
  board_selected <- reactive({if(input$board != "Please select"){TRUE}else{FALSE}})
  
  output$team_difference <- renderUI({
    
    if(board_selected()){
      if(abs(team_data$difference) > 0 && sign(team_data$difference) == 1){
        text <- markdown(paste0("Across your health board, you have  **", team_data$after_count, "** teams, that's an increase of ", team_data$difference, " since last year!"))
      } else if (abs(team_data$difference) > 0 && sign(team_data$difference) == -1){
        text <- markdown(paste0("Across your health board, you have  **", team_data$after_count, "** teams, that's a change of ", team_data$prop, " since last year!"))
      } else{
        text <- markdown(paste0("Across your health board, your team count has been stable, you have the same numbner of teams this year as last, **", team_data$after_count, "**."))
      }
    } else{
      text <- "Choose your board to learn about teams in your health board"
    }
    return(text)
  })
  
  output$measure_difference <- renderUI({
    
    if(board_selected()){
      if(abs(measure_counts$difference) > 0 && sign(measure_counts$difference) == 1){
        text <- markdown(paste0("Across your health board, you have  **", measure_counts$after_count, "** teams, that's an increase of ", measure_counts$difference, " since last year!"))
      } else if (abs(measure_counts$difference) > 0 && sign(measure_counts$difference) == -1){
        text <- markdown(paste0("Across your health board, you have  **", measure_counts$after_count, "** teams, that's a change of ", measure_counts$prop, " since last year!"))
      } else{
        text <- markdown(paste0("Across your health board, your team count has been stable, you have the same numbner of teams this year as last, **", measure_counts$after_count, "**."))
      }
    } else{
      text <- "Choose your board to learn about measures in your health board"
    }
    return(text)
  })
  
  
}
