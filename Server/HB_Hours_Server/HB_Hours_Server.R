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
  
  output$Total_WTE <- renderUI({
    
    if(board_selected()){
      
      data <- WTE_data()
      text_to_show <- sum(data$hours, na.rm = T)
      text_to_show <- round(text_to_show, 1)
        
        box <- value_box(
          title = "Total WTE",
          showcase = bs_icon("person-arms-up"),
          value = text_to_show,
          theme = value_box_theme(bg = "#C73918"), # PHS Rust
          p("The total baseline hours your team worked")
        )
    } else{
      box <- value_box(
        showcase = bs_icon("person-arms-up"),
        value = "Select your health board",
        theme = value_box_theme(bg = "#C73918"), # PHS Rust
        p("and learn about WTE staffing in your health board!")
      )
    }
    return(box)
  })
  
  
  output$WTE_hours <- renderUI({
    
    if(board_selected()){
      
      output$WTE_plot <-  renderPlot({
        
        data <- WTE_data()
        
        data |> 
          ggplot(aes(measure_date, hours), colour = input$board, group = input$board) + 
          geom_point() +
          geom_line() +
          scale_y_continuous(labels = scales::label_comma()) +
          phsstyles::theme_phs()
        
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
  
  output$Total_BA <- renderUI({
    
    if(board_selected()){
      
      data <- SSUBA_data()
      text_to_show <- sum(data$hours, na.rm = T)
      text_to_show <- round(text_to_show, 1)
      
      box <- value_box(
        title = "Total B&A",
        showcase = bs_icon("hourglass"),
        value = text_to_show,
        theme = value_box_theme(bg = "#83BB26"), # PHS Green
        p("The total bank and agency hours your team had")
      )
    } else{
      box <- value_box(
        showcase = bs_icon("hourglass"),
        value = "Select your health board",
        theme = value_box_theme(bg = "#83BB26"), # PHS Green
        p("and learn about teams in your health board!")
      )
        
    }
    return(box)
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
  
  output$Total_EO <- renderUI({
    
    if(board_selected()){
      
      data <- SSUEO_data()
      text_to_show <- sum(data$hours, na.rm = T)
      text_to_show <- round(text_to_show, 1)
      
      box <- value_box(
        title = "Total E&O",
        showcase = bs_icon("clipboard-heart"), 
        value = text_to_show,
        theme = value_box_theme(bg = "#9B4393"), # PHS Magenta 
        p("The total bank and agency hours your team had")
      )

    } else{
      box <- value_box(
        showcase = bs_icon("hourglass"),
        value = "Select your health board",
        theme = value_box_theme(bg = "#9B4393"), # PHS Magenta 
        p("and learn about teams in your health board!")
      )
      
    }
    return(box)
  })
  
}