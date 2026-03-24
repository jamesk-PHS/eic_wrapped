Streaks_Server <- function(input, output, session) {
  
  output$submission_streak_plot <- renderUI({
    
    if(input$board_sub_data != "Please select"){
      
      output$sub_plot <-  renderPlot({
        
        data <- rbind(eicmethods::submission_data("team"),
                      eicmethods::submission_data("team partial")) |> 
          filter(hb_name == input$board_sub_data) |> 
          arrange(hb_name, sub_location_code, sub_location_name, measure_id, month) |> 
          group_by(hb_name, sub_location_code, measure_id) |> 
          mutate(streak = accumulate(n_submitted, # Iterate through NRV_check 
                                     ~ if(.y == 1){ # If the next observation is true
                                       .x + 1 # Add 1 to the last observation
                                     }else{0}, # Otherwise, reset to 0
                                     .init = 0)[-1]) |>  # Start with 0 and [-1] drop the initial condition so the vector lengths match
          mutate(max_streak = max(streak, na.rm = T))
        
        data |> 
          mutate(num_of_subs = max(row_number(), na.rm = T)) |>
          filter(max_streak >= 0.8*num_of_subs) |> 
          ungroup() |> 
          distinct(sub_location_code, sub_location_name) |> 
          ggplot(aes(label = sub_location_name)) + 
          ggwordcloud::geom_text_wordcloud()
        
      })
      
      return(plotOutput("sub_plot")) 
      
    } else{
      output$sub_text <- renderText({"Choose your board to learn about teams in your health board"})
      return(textOutput("sub_text"))
    }
  })
  
  output$NRV_streak_plot <- renderUI({
    
    if(input$board_NRV_data != "Please select"){
      
      output$NRV_plot <-  renderPlot({
        
        tde_exract_tallied |> 
          filter(hb_name == input$board_NRV_data) |> 
          mutate(num_of_subs = max(row_number(), na.rm = T)) |>
          filter(max_streak >= 0.8*num_of_subs) |> 
          ungroup() |> 
          distinct(sub_location_code, sub_location_name) |> 
          ggplot(aes(label = sub_location_name)) + 
          ggwordcloud::geom_text_wordcloud()
        
        
      })
      
      return(plotOutput("NRV_plot")) 
      
    } else{
      output$NRV_text <- renderText({"Choose your board to learn about teams in your health board"})
      return(textOutput("NRV_text"))
    }
  })
  
  
  
}