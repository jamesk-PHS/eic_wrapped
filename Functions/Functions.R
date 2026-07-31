
# first_last_submission_data counts the number of monthly submissions a 
# HB has made in CAIR over its tenure. It first selects a monthly count, 
# then, based on that data, adds up all monthly data into a rolling cumulative sum
first_last_submission_data <- function(board) {
  
  reactive({
    
    query <- paste0("WITH monthly_counts AS (SELECT health_board_name, TRUNC(measure_date, 'MM') AS measure_date, COUNT(*) AS row_count
                                                  FROM eic.eic_all_data
                                                  WHERE measure_date >= DATE '2016-04-01' AND health_board_name = '", board, "'
                                                  GROUP BY health_board_name, TRUNC(measure_date, 'MM'))
                              SELECT health_board_name, measure_date, row_count, SUM(row_count)  OVER (PARTITION BY health_board_name ORDER BY measure_date) AS cum_sum
                              FROM monthly_counts
                              ORDER BY health_board_name, measure_date")
    
    data <- tbl(dv_con, sql(query)) |>
      collect()
    
    return(data)
  })
}

# count_distinct counts the number of distinct rows within a variable.
# It's used to count teams and measure changes between financial years.
count_distinct <- function(board, col_to_count) {
  
  reactive({
    
    query <- paste0(
      "SELECT
         health_board_name,
         COUNT(DISTINCT CASE
           WHEN measure_date >= DATE '2023-04-01'
            AND measure_date <  DATE '2024-04-01'
           THEN ", col_to_count, "
         END) AS before_count,
         COUNT(DISTINCT CASE
           WHEN measure_date >= DATE '2024-04-01'
            AND measure_date <  DATE '2025-04-01'
           THEN ", col_to_count, "
         END) AS after_count
       FROM eic.eic_all_data
       WHERE health_board_name = '", board, "'
       GROUP BY health_board_name"
    )
    
    data <- tbl(dv_con, sql(query)) |>
      collect() |>
      mutate(
        difference = after_count - before_count,
        prop_change = (1 - (after_count / before_count)) * 100,
        prop_change = replace_na(prop_change, 0),
        prop_change = round(prop_change, 0),
        prop_change = paste0(prop_change, "%")
      )
    
    return(data)
    
  })
}

# cair_representation tallies the proportion of a HB's CAIR data as a % of the 
# total data on CAIR.
cair_representation <-  function(board){
  
  reactive({
    
    query <- paste0("WITH total_count AS (
      SELECT COUNT(*) AS total_row_count
      FROM eic.eic_all_data
    ),
    board_count AS (
      SELECT
        health_board_name,
        COUNT(*) AS board_row_count
      FROM eic.eic_all_data
      WHERE health_board_name = '", board, "'
      GROUP BY health_board_name
    )
    SELECT
      b.health_board_name,
      b.board_row_count,
      t.total_row_count,
      (b.board_row_count / t.total_row_count) * 100.0 AS board_perc
    FROM board_count b
    CROSS JOIN total_count t
  ")
    
    data <- tbl(dv_con, sql(query)) |>
      collect()
    
    return(data)
  })
  
}

# WTE_data calculates the total monthly WTE data. It starts by creating a 
# a table called monthly_counts which is the monthly total.
# Then, using that table, creates a rolling cumulative sum across the HB. 
WTE_data <-  function(board){
  
  reactive({
    
    query <- paste0(query <- "WITH monthly_counts AS (
    SELECT
        health_board_name,
        TRUNC(measure_date, 'MM') AS measure_date,
        SUM(
            CAST(user_data_1 AS DECIMAL) + -- non-registered WTE
            CAST(user_data_2 AS DECIMAL) -- registered WTE
        ) AS est_sum
    FROM eic.eic_all_data
    WHERE measure_id = 'EST' AND health_board_name = '", board, "'
    GROUP BY
        health_board_name,
        TRUNC(measure_date, 'MM')
)

SELECT
    health_board_name,
    measure_date,
    est_sum,
    SUM(est_sum) OVER (
        PARTITION BY health_board_name
        ORDER BY measure_date
        ROWS UNBOUNDED PRECEDING
    ) AS cum_sum
FROM monthly_counts -- The table we've just queried above^
ORDER BY
    health_board_name,
    measure_date
")
  
  data <- tbl(dv_con, sql(query)) |>
    collect()
  
  return(data)
  })

}


# work_force_query is probably one of the most complex SQL queries in this list.
# This is because the submission format of various workforce measures are different.
work_force_query <- function(board){
  
  reactive({
    
    query_1 <- paste0("SELECT health_board_name, 
                        TRUNC(measure_date, 'MM') AS measure_date, 
                        measure_id,
                        SUM(CAST([user_data_1] AS DECIMAL)) AS non, 
                        SUM(CAST([user_data_2] AS DECIMAL)) AS reg
  FROM eic.eic_all_data
  WHERE measure_id IN ('EST', 'SSUB', 'SSUA') AND health_board_name = '", board,"'
  GROUP BY health_board_name, measure_id, measure_date
  ORDER BY health_board_name, measure_id, measure_date")
    
    
    data_frame_a <- tbl(dv_con, sql(query_1)) |> 
      collect() |>
      pivot_longer(cols = c("non", "reg"), names_to = "type", values_to = "value") |> 
      pivot_wider(names_from = "measure_id", values_from = "value", names_sep = "_")
    
    
    
    query_2 <- paste0("SELECT health_board_name, 
                        TRUNC(measure_date, 'MM') AS measure_date, 
                        measure_id,
                        LOWER(SUBSTR(user_data_1, 1, 3)) AS type, 
                        SUM(CAST([user_data_3] AS DECIMAL)) AS SSUEO_excess, 
                        SUM(CAST([user_data_4] AS DECIMAL)) AS SSUEO_OT
   FROM eic.eic_all_data
   WHERE measure_id = 'SSUEO'  AND health_board_name = '", board,"' -- SSUEO has an odd format compared to the other workforce measures
   GROUP BY health_board_name, measure_date, measure_id, type
   ORDER BY health_board_name, measure_date, measure_id, type")
    
    data_frame_b <- tbl(dv_con, sql(query_2)) |> 
      collect() |> 
      select(-measure_id)
    
    # Before we can join then, we need to get some parameters since
    # workforce data reporting patterns can differ both between and
    # within a HB.
    min_date <- sort(c(first(data_frame_a$measure_date), first(data_frame_b$measure_date)), decreasing = FALSE)[1]
    max_date <- sort(c(last(data_frame_a$measure_date), last(data_frame_b$measure_date)), decreasing = TRUE)[1]
    health_board_name <- sort(c(unique(data_frame_a$health_board_name), unique(data_frame_b$health_board_name)), decreasing = TRUE)[1]
    
    # Based on the above, build a skeleton data frame and join the data on
    expand_grid(health_board_name = health_board_name, 
                measure_date = seq(min_date, max_date, by = "months"), 
                type = c("non", "reg")) |> 
      left_join(data_frame_a) |> 
      left_join(data_frame_b)
    
  })
  
  
  
}


NRV_streaks <- function(board){
  
  reactive({
    
    query <- paste0("WITH meta_data AS (
    SELECT DISTINCT
        measure_id,
        CAST(reference_point AS DECIMAL) AS ref_point
    FROM eic.eic_all_data
    WHERE reference_point IS NOT NULL
),
 
calculated_data AS (
    SELECT
        hb_name,
        TRUNC(measure_date_my, 'MM') AS measure_date_my,
        sub_location_code,
        measure_id,
        CAST(calc_rate AS DECIMAL) AS calc_rate,
        high_rate_is_better_flag
    FROM eic.eic_overview
    WHERE hb_name = '", board,"'
),
 
joined_data AS (
    SELECT
        c.*,
        m.ref_point,
        CASE
            WHEN calc_rate >= ref_point AND high_rate_is_better_flag = 1 THEN 1
            WHEN calc_rate <= ref_point AND high_rate_is_better_flag = 0 THEN 1
            ELSE 0
        END AS success_flag
    FROM calculated_data c
    LEFT JOIN meta_data m
        ON c.measure_id = m.measure_id
),
 
groups AS (
    SELECT
        *,
        SUM(CASE WHEN success_flag = 0 THEN 1 ELSE 0 END)
            OVER (
                PARTITION BY hb_name, measure_id, sub_location_code
                ORDER BY measure_date_my
            ) AS grp
    FROM joined_data
)
 
SELECT
    *,
    CASE
    WHEN success_flag = 1 THEN
        SUM(success_flag) OVER (
            PARTITION BY hb_name,
                         measure_id,
                         sub_location_code,
                         grp
            ORDER BY measure_date_my
            ROWS UNBOUNDED PRECEDING
        )
    ELSE 0
END AS success_streak
FROM groups
ORDER BY
    hb_name,
    measure_id,
    sub_location_code,
    measure_date_my")
  
  data <- tbl(dv_con, sql(query)) |>
    collect()
  
  return(data)
  })

}



run_chart_streaks <- function(board){
  
  reactive({
    
    query <- paste0("WITH team_aggregate_baseline AS (
    SELECT
        hb_name,
        sub_location_code,
        measure_id,
        COUNT(DISTINCT phase) AS distinct_rephases
    FROM eic.eic_overview
    WHERE phase = '2' AND hb_name = '", board,"'
    GROUP BY hb_name, sub_location_code, measure_id
),

baseline_data AS (
    SELECT
        hb_name,
        SUM(distinct_rephases) AS total_rephases
    FROM team_aggregate_baseline
    GROUP BY hb_name
),

team_aggregate_trend AS (
    SELECT
        hb_name,
        sub_location_code,
        measure_id,
        COUNT(trend) AS distinct_trend
    FROM eic.eic_overview
    WHERE trend IS NOT NULL AND hb_name = '", board,"'
    GROUP BY hb_name, sub_location_code, measure_id
),

trend_data AS (
    SELECT
        hb_name,
        SUM(distinct_trend) AS total_trends
    FROM team_aggregate_trend
    GROUP BY hb_name
),

team_aggregate_shift AS (
    SELECT
        hb_name,
        sub_location_code,
        measure_id,
        COUNT(shift) AS distinct_shift
    FROM eic.eic_overview
    WHERE shift IS NOT NULL AND hb_name = '", board,"'
    GROUP BY hb_name, sub_location_code, measure_id
),

shift_data AS (
    SELECT
        hb_name,
        SUM(distinct_shift) AS total_shifts
    FROM team_aggregate_shift
    GROUP BY hb_name
)

SELECT
    b.hb_name,
    b.total_rephases,
    t.total_trends,
    s.total_shifts
FROM baseline_data b
LEFT JOIN trend_data t
    ON b.hb_name = t.hb_name
LEFT JOIN shift_data s
    ON b.hb_name = s.hb_name
ORDER BY b.hb_name")
    
    data <- tbl(dv_con, sql(query)) |>
      collect()
    
    return(data)
  })
  
}

# Catch all UI element shown until user selects parameters
null_box <- function(){
  
  value_box(
    title = "Select your health board",
    value = "Learn about your historic CAIR data",
    showcase = bs_icon("patch-question"),
    theme = value_box_theme(bg = "#DFDDE3") # PHS Rust
  )
  
}


