HB_Server <- function(input, output, session){
  
  board_selected <- reactive({if(input$board != "Please select"){TRUE}else{FALSE}})
  
  output$team_difference <- renderUI({
    
    if(board_selected()){
      
      if(abs(team_data$difference) > 0 && sign(team_data$difference) == 1){
        text_to_show <- paste0("teams, that's an increase of ", team_data$difference, " since last year!")
        
        box <- value_box(
          title = "Across your health board, you have",
          showcase = bs_icon("people-fill"),
          value = team_data$after_count,
          theme = value_box_theme(bg = "#3F3685"), # PHS Purple
          p(text_to_show)
        )
        
        
      } else if (abs(team_data$difference) > 0 && sign(team_data$difference) == -1){
        text_to_show <- paste0("teams, that's a change of ", team_data$prop, " since last year!")
        
        box <- value_box(
          title = "Across your health board, you have",
          showcase = bs_icon("people-fill"),
          value = team_data$after_count,
          theme = value_box_theme(bg = "#3F3685"), # PHS Purple
          p(text_to_show)
        )
        
      } else{
        text_to_show <- paste0("teams, the same number of teams this year as last, ", team_data$after_count, ".")
        
        box <- value_box(
          title = "Across your health board, you have",
          showcase = bs_icon("people-fill"),
          value = team_data$after_count,
          theme = value_box_theme(bg = "#3F3685"), # PHS Purple
          p(text_to_show)
        )
      }
    } else{
      text_to_show <- "and learn about teams in your health board!"
      
      box <- value_box(
        value = "Select your health board",
        showcase = bs_icon("people-fill"),
        theme = value_box_theme(bg = "#3F3685"), # PHS Purple
        p(text_to_show)
      )
    }
    return(box)
  })
  
  output$measure_difference <- renderUI({
    
    if(board_selected()){
      if(abs(measure_counts$difference) > 0 && sign(measure_counts$difference) == 1){
        text_to_show <- paste0("measures, that's an increase of ", measure_counts$difference, " since last year!")

        box <- value_box(
          title = "Across your health board, you have",
          showcase = bs_icon("activity"),
          value = measure_counts$after_count,
          theme = value_box_theme(bg = "#0078D4"), # PHS Blue
          p(text_to_show)
        )
        
      } else if (abs(measure_counts$difference) > 0 && sign(measure_counts$difference) == -1){
        text_to_show <- paste0("measures, that's a change of ", measure_counts$prop, " since last year!")
        
        box <- value_box(
          title = "Across your health board, you have",
          showcase = bs_icon("activity"),
          value = measure_counts$after_count,
          theme = value_box_theme(bg = "#0078D4"), # PHS Blue
          p(text_to_show)
        )
        
      } else{
        text_to_show <- paste0("the same numbner of measures this year as last, ", measure_counts$after_count, ".")
        
        box <- value_box(
          title = "Across your health board, you have",
          showcase = bs_icon("activity"),
          value = measure_counts$after_count,
          theme = value_box_theme(bg = "#0078D4"), # PHS Blue
          p(text_to_show)
        )
        
      }
    } else{
      text_to_show <- "and learn about measures in your health board"
      
      box <- value_box(
        value = "Select your health board",
        showcase = bs_icon("activity"),
        theme = value_box_theme(bg = "#0078D4"), # PHS Blue
        p(text_to_show)
      )
    }
    return(box)
  })
  
  
}
