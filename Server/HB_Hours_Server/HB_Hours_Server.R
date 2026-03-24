HB_Hours_Server <- function(input, output, session) {
  
  board_selected <- reactive({if(input$board != "Please select"){TRUE}else{FALSE}})
  
  WTE_data <- reactive({
    
    req(board_selected())
    
    eicmethods::raw_submission_data() |> 
      filter(measure_id == "EST",
             health_board_name == input$board) |>
      rename("nonregistered_est" = user_data_1, "registered_est" = user_data_2) |>
      select(-contains("user_data"), -measure_id) |>
      mutate(
        nonregistered_est = as.numeric(nonregistered_est),
        registered_est = as.numeric(registered_est),
        overall_est = nonregistered_est+registered_est,
        hours = overall_est*37.5) |> 
      group_by(health_board_name, measure_date) |> 
      summarise(hours = sum(hours, na.rm = T), .groups = "drop")
  })
  
  output$total_WTE <- renderText({
    
    if(board_selected()){
      data <- WTE_data()
      text <- sum(data$hours, na.rm = T)
      text <- round(text, 1)
      text <- as.character(text)
    } else{
      text <- "Choose your HB"
    }
    return(text)
    
  })
  
  output$WTE_hours <- renderUI({
    
    if(board_selected()){
      
      output$WTE_plot <-  renderPlot({
        
        data <- WTE_data()
        
        data |> 
          ggplot(aes(measure_date, hours)) + 
          geom_line()
        
      })
      
      return(plotOutput("WTE_plot")) 
      
    } else{
      output$WTE_text <- renderText({"Choose your board to learn about teams in your health board"})
      return(textOutput("WTE_text"))
    }
  })
  
  SSUBA_data <- reactive({
    
    req(board_selected())
    
    eicmethods::raw_submission_data() |> 
      filter(measure_id %in% c("SSUA", "SSUB"),
             health_board_name == input$board) |>
      rename(nonregistered_ba = user_data_1, registered_ba = user_data_2) |>
      mutate(across(.cols = c("nonregistered_ba", "registered_ba"), ~as.numeric(.x))) |>
      group_by(health_board_name, sub_location_code, measure_date) |>
      summarise(nonregistered_ba = sum(nonregistered_ba), 
                registered_ba = sum(registered_ba),
                total_ba = nonregistered_ba + registered_ba)
  })
  
  output$total_SSUBA <- renderText({
    
    if(board_selected()){
      data <- SSUBA_data()
      text <- sum(data$total_ba, na.rm = T)
      text <- round(text, 1)
      text <- as.character(text)
    } else{
      text <- "Choose your HB"
    }
    return(text)
    
  })
  
  
  SSUEO_data <- reactive({
    
    req(board_selected())
    
    eicmethods::raw_submission_data() |> 
      filter(measure_id %in% c("SSUEO"),
             health_board_name == input$board) |>  
      rename("type" = user_data_1, "excess" = user_data_3, "overtime" = user_data_4) |>
      select(-contains("user_data")) |>
      mutate(type = str_to_lower(type),
             type = str_replace(type, " ", "_"),
             across(c("excess", "overtime"), as.numeric)) |> 
      group_by(health_board_name, sub_location_code, measure_date) |>
      summarise(type = first(type), 
                excess = sum(excess),
                overtime = sum(overtime)) |> 
      pivot_wider(names_from = "type", values_from = c("overtime", "excess"), names_sep = "_", values_fill = 0) |> 
      group_by(health_board_name, measure_date) |> 
      summarise(across(contains("registered"), sum)) |> 
      mutate(total_eo = sum(3:6))
  }) 
  output$total_SSUEO <- renderText({
    
    if(board_selected()){
      data <- SSUEO_data()
      text <- sum(pull(data, hours), na.rm = T)
      text <- round(text, 1)
      text <- as.character(text)
    } else{
      text <- "Choose your HB"
    }
    return(text)
  })
  
}