

# Dataset 1 ---------------------------------------------------------------

team_data <- tbl(channel,
                 sql("SELECT health_board_code,
    COUNT(DISTINCT CASE 
        WHEN measure_date >= '2023-04-01'
         AND measure_date <  '2024-04-01'
        THEN sub_location_code
    END) AS before_count,
    COUNT(DISTINCT CASE 
        WHEN measure_date >= '2024-04-01'
         AND measure_date <  '2025-04-01'
        THEN sub_location_code
    END) AS after_count 
FROM eic.eic_measure_data
GROUP BY health_board_code
")
) |>
  collect() |> 
  mutate(difference = after_count - before_count,
         prop_change = (1-(after_count/before_count))*100)


# Dataset 2 ---------------------------------------------------------------

measure_counts <- tbl(channel,
                      sql("SELECT health_board_code,
    COUNT(DISTINCT CASE 
        WHEN measure_date >= '2023-04-01'
         AND measure_date <  '2024-04-01'
        THEN measure_id
    END) AS before_count,
    COUNT(DISTINCT CASE 
        WHEN measure_date >= '2024-04-01'
         AND measure_date <  '2025-04-01'
        THEN measure_id
    END) AS after_count 
FROM eic.eic_measure_data
GROUP BY health_board_code
")
) |>
  collect() |> 
  mutate(difference = after_count - before_count,
         prop_change = (1-(after_count/before_count))*100)

dbDisconnect(channel)


# Dataset 3 ---------------------------------------------------------------

boards <- c("Please select", unique(eicmethods::raw_submission_data()$health_board_name))