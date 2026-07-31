server <- function(input, output, session) {
  
  source("Functions/Functions.R")
  

  
  # Main breaker for when/how page renders data
  board_selected_flag <- reactive({if(input$board != "Please select"){TRUE}else{FALSE}})
  
  submission_data <- reactive({
    
    req(board_selected_flag())
    
    first_last_submission_data(input$board)()
  })
  
  
  # card: CAIR teams
  
  output$First_Sub <- renderUI({
    if(board_selected_flag()){
      
      box <- value_box(
        title = "Your CAIR Jounrey began...",
        showcase = bs_icon("calendar-heart"),
        value = first(format(submission_data()$measure_date, format = "%d %B %Y")),
        theme = value_box_theme(bg ="#3F3685"), # PHS Rust
        p(glue::glue("That month, you submitted {head(submission_data()$row_count, n = 1)} rows of data"))
      )
    } else{
      
      box <- null_box()
      
    }
    return(box)
  })
  
  output$Latest_Sub <- renderUI({
    
    if(board_selected_flag()){
      
      text_to_show <- prettyNum(last(submission_data()$cum_sum), big.mark=",", scientific=FALSE) 
      
      box <- value_box(
        title = "Since then...",
        showcase = bs_icon("bar-chart-line-fill"),
        value = text_to_show,
        theme = value_box_theme(bg ="#9B4393"), # PHS Rust
        p(glue::glue("Look how much data you've shared since!"))
      )
    } else{
      
      box <- null_box()
    }
    return(box)
  })#
  
  
  output$CAIR_representation <- renderUI({
    
    if(board_selected_flag()){
      
      data <- cair_representation(input$board)()
      text_to_show <- glue::glue("{data$board_perc}%") 
      text_to_embed <- prettyNum(data$total_row_count, big.mark=",", scientific=FALSE) 
      
      box <- value_box(
        title = "Your board accounts for",
        showcase = bs_icon("bar-chart-line-fill"),
        value = text_to_show,
        theme = value_box_theme(bg ="#9B4393"), # PHS Rust
        p(glue::glue("Of all {text_to_embed} data on CAIR!"))
      )
    } else{
      
      box <- null_box()
    }
    return(box)
  })
  
  output$Change_Team <- renderUI({
    
    if(board_selected_flag()){
      
      data <- count_distinct(input$board, "sub_location_code")()
      text_to_show <- data$prop_change 
      
      box <- value_box(
        title = "Change teams",
        showcase = bs_icon("person-arms-up"),
        value = text_to_show,
        theme = value_box_theme(bg ="#83BB26"), # PHS Rust
        p(glue::glue("This year, you had {data$after_count} teams with data on CAIR, that's a \n
                     {text_to_show} change compared to the previous financial year."))
      )
    } else{
      box <- null_box()
    }
    return(box)
  })
  
  
  output$Change_Measure <- renderUI({
    
    if(board_selected_flag()){
      
      data <- count_distinct(input$board, "measure_id")()
      text_to_show <- data$prop_change 
      
      box <- value_box(
        title = "Change teams",
        showcase = bs_icon("search"),
        value = text_to_show,
        theme = value_box_theme(bg ="#83BB26"), # PHS Rust
        p(str_glue("This year, you had {data$after_count} teams with data on CAIR, that's a \n
                     {text_to_show} change compared to the previous financial year."))
      )
    } else{
      box <- null_box()
    }
    return(box)
  })
  
  output$Cumulative_subs <- renderPlot({
    
    if(board_selected_flag()){
      
      data <- submission_data()
      
      plot <- ggplot(data, aes(measure_date, cum_sum, group = health_board_name)) + 
        geom_line(colour = phsstyles::phs_colors("phs-purple"), linewidth = 1.25) +
        scale_y_continuous(labels = scales::label_comma()) +
        scale_x_date(labels = scales::label_date_short()) +
        theme_phs()

    } else{
      
      plot <- ggplot(data = NULL, aes(x = c(1:10), y = c(1:10))) + 
        theme_minimal() + 
        scale_y_continuous(breaks = NULL, labels = NULL) +
        scale_x_continuous(breaks = NULL, labels = NULL) 
      
    }
    
    plot <- plot +
      labs(x = "Date",
           y = "Submission count") 
      
    return(plot)
  })
  
  
  output$Total_WTE <- renderUI({
    
    if(board_selected_flag()){
      
      data <- WTE_data(input$board)()
      text_to_show <- last(data$cum_sum)
      
      box <- value_box(
        title = "Total WTE",
        showcase = bs_icon("person-arms-up"),
        value = text_to_show,
        theme = value_box_theme(bg = "#C73918"), # PHS Rust
        p("The total baseline hours your team worked")
      )
    } else{
      box <- null_box()
    }
    return(box)
  })
  
  
  output$WTE_hours <- renderUI({
    
    if(board_selected_flag()){
      
      output$WTE_plot <-  renderPlot({
        
        data <- WTE_data(input$board)()
        
        data |> 
          ggplot(aes(measure_date, cum_sum), colour = input$board, group = input$board) + 
          geom_point(size = 1.25) +
          geom_line(linewidth = 1.25) +
          scale_y_continuous(labels = scales::label_comma()) +
          scale_x_date(labels = scales::label_date_short()) +
          phsstyles::theme_phs()
        
      })
      
      return(plotOutput("WTE_plot")) 
      
    } else{
      output$WTE_text <- renderText({"Choose your board to learn about teams in your health board"})
      return(textOutput("WTE_text"))
    }
  })
  
  workforce_data <- work_force_query(input$board)

  output$Total_BA <- renderUI({
    
    if(board_selected_flag()){
      
      text_to_show <- sum(workforce_data()$SSUA, workforce_data()$SSUB, na.rm = T) 
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
  
 
  output$Total_EO <- renderUI({
    
    if(board_selected_flag()){
      
      text_to_show <- sum(workforce_data()$ssueo_excess, workforce_data()$ssueo_ot, na.rm = T)
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
  
  output$Cumulative_WTE <- renderPlot({
    
    if(board_selected_flag()){
      
      data <- workforce_data()
      
      plot <- ggplot(data, aes(measure_date, EST, group = type, colour = type)) + 
        geom_point(size = 1.25) +
        geom_line(linewidth = 1.25) +
        scale_y_continuous(labels = scales::label_comma()) +
        scale_x_date(labels = scales::label_date_short()) +
        scale_color_manual(values = c("#3F3685", "#9B4393")) + 
        phsstyles::theme_phs()
      
    } else{
      
      plot <- ggplot(data = NULL, aes(x = c(1:10), y = c(1:10))) + 
        theme_minimal() + 
        scale_y_continuous(breaks = NULL, labels = NULL) +
        scale_x_continuous(breaks = NULL, labels = NULL) 
      
    }
    
    plot <- plot +
      labs(x = "Date",
           y = "Submission count") 
    
    return(plot)
  })
  
  output$Streak_NRV <- renderUI({
    
    if(board_selected_flag()){
      
      data <- NRV_streaks(input$board)()
      text_to_show <- max(data$success_streak)
      
      box <- value_box(
        title = "NRV Streaks",
        showcase = bs_icon("star"), 
        value = text_to_show,
        theme = value_box_theme(bg = "#9B4393"), # PHS Magenta 
        p("The longest streak a team has had being better than or qual to
          the national reference for a measure.")
      )
      
    } else{
      box <-  null_box()
      
    }
    return(box)
  })
  
  run_chart_streaks_data <- run_chart_streaks(input$board)
  
  
  output$Streak_rephases <- renderUI({
    
    if(board_selected_flag()){
      
      text_to_show <- run_chart_streaks_data()$total_rephases
      
      box <- value_box(
        title = "REPHASES",
        showcase = bs_icon("star"), 
        value = text_to_show,
        theme = value_box_theme(bg = "#9B4393"), # PHS Magenta 
        p("The number of rephases on the dashboard")
      )
      
    } else{
      box <-  null_box()
      
    }
    return(box)
  })
  
  output$Streak_trends <- renderUI({
    
    if(board_selected_flag()){
      
      text_to_show <- run_chart_streaks_data()$total_trends
    
      box <- value_box(
        title = "TRENDS",
        showcase = bs_icon("star"), 
        value = text_to_show,
        theme = value_box_theme(bg = "#9B4393"), # PHS Magenta 
        p("The number of team on the dashboard")
      )
      
    } else{
      box <-  null_box()
      
    }
    return(box)
  })
  
  output$Streak_shifts <- renderUI({
    
    if(board_selected_flag()){
      
      text_to_show <- run_chart_streaks_data()$total_shifts
      
      box <- value_box(
        title = "SHFITS",
        showcase = bs_icon("star"), 
        value = text_to_show,
        theme = value_box_theme(bg = "#9B4393"), # PHS Magenta 
        p("The number of shifts on the dashboard")
      )
      
    } else{
      box <-  null_box()
      
    }
    return(box)
  })
  
  
  
}