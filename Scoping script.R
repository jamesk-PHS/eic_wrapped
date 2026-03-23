

library(tidyverse)
library(eicmethods)

# Team change -------------------------------------------------------------

tde_extract <- arrow::read_parquet("data/Overview TDE - data table (4).parquet") |> 
  janitor::clean_names() |> 
  as_tibble()

tde_extract |> 
  mutate(measure_date_my = dmy(measure_date_my),
         fin_year = phsmethods::extract_fin_year(measure_date_my),
         year = str_sub(fin_year, 1, 4),
         year = as.numeric(year)) |>
  filter(year == max(year) | year == max(year)-1) |> 
  select(year, hb_name, location_code, location_name, sub_location_code) |> 
  distinct() |> 
  group_by(year, hb_name) |> 
  summarise(sub_location_code = length(sub_location_code)) |> 
  pivot_wider(names_from = "year", values_from = "sub_location_code") |> 
  mutate(diff = `2025` - `2024`) |> 
  arrange(desc(diff))

# SQL Query :

tbl(channel,
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

# Streaks -----------------------------------------------------------------
## Longest submission ----------------------------------------------------


rbind(eicmethods::submission_data("team"),
      eicmethods::submission_data("team partial"))|> 
  arrange(hb_name, sub_location_code, sub_location_name, measure_id, month) |> 
  group_by(hb_name, sub_location_code, sub_location_name, measure_id) |> 
  mutate(streak = accumulate(n_submitted, # Iterate through NRV_check 
                             ~ if(.y == 1){ # If the next observation is true
                               .x + 1 # Add 1 to the last observation
                             }else{0}, # Otherwise, reset to 0
                             .init = 0)[-1]) |>  # Start with 0 and [-1] drop the initial condition so the vector lengths match
  mutate(max_streak = max(streak))


## Longest NRV -----------------------------------------------------------

tde_exract_expanded <- tde_extract |> 
  mutate(measure_date_my = dmy(measure_date_my),
         across(9:11, ~str_remove_all(.x, ",")), # remove commas from numbers
         across(9:11, as.numeric)) |> 
  left_join(eicmethods::return_referece_file("REFPOINT") |> #  Attach NRV data
              distinct(measureid, refpoint),
            by = c("measure_id" = "measureid")) |> 
  left_join(eicmethods::ref_points_guidance, # Attach NRV context
            by = "measure_id")


tde_exract_expanded_true_zeros <- tde_exract_expanded |> 
  filter(numerator == 0 | is.na(calc_rate)) |> 
  mutate(NRV_check = FALSE)



tde_exract_expanded_true_rates <- tde_exract_expanded |> 
  filter(numerator != 0) |> 
  mutate(NRV_check = case_when(ref_threshold == "higher" & calc_rate >= refpoint ~ TRUE,
                               ref_threshold == "lower" & calc_rate <= refpoint ~ TRUE,
                               .default = FALSE))


tde_exract_expanded <- rbind(tde_exract_expanded_true_rates, tde_exract_expanded_true_zeros)

rm(tde_exract_expanded_true_rates, tde_exract_expanded_true_zeros)

tde_exract_tallied <- tde_exract_expanded |> 
  arrange(hb_name, sub_location_code, sub_location_name, measure_id, measure_date_my) |> 
  group_by(hb_name, sub_location_code, sub_location_name, measure_id) |> 
  mutate(streak = accumulate(NRV_check, # Iterate through NRV_check 
                             ~ if(.y == TRUE){ # If the next observation is true
                               .x + 1 # Add 1 to the last observation
                             }else{0}, # Otherwise, reset to 0
                             .init = 0)[-1]) |>  # Start with 0 and [-1] drop the initial condition so the vector lengths match
  mutate(max_streak = max(streak))



rbenchmark::benchmark(
  "ggwordcloud" =  {
    
    tde_exract_tallied |> 
      mutate(num_of_subs = max(row_number())) |>
      filter(str_detect(hb_name, "ARRAN"),
             max_streak >= 0.8*num_of_subs) |> 
      distinct(sub_location_code, sub_location_name, measure_id, max_streak) |> 
      ggplot(aes(label = sub_location_name)) + 
      ggwordcloud::geom_text_wordcloud()
  },
  
  "wordlcoud" =  {
    tde_exract_tallied |> 
      mutate(num_of_subs = max(row_number())) |>
      filter(str_detect(hb_name, "ARRAN"),
             max_streak >= 0.8*num_of_subs) |> 
      distinct(sub_location_code, sub_location_name, measure_id, max_streak) |> 
      with(wordcloud::wordcloud(sub_location_name))
  },
  replications = 10
  )

## ggwordcloud is faster by about (1-1/1.168) or 14%.


# Hours worked ------------------------------------------------------------



eicmethods::raw_submission_data() |> 
  filter(measure_id %in% c("SSUA", "SSUB")) |>
  rename(nonregistered_ba = user_data_1, registered_ba = user_data_2) |>
  mutate(across(.cols = c("nonregistered_ba", "registered_ba"), ~as.numeric(.x))) |>
  group_by(health_board_name, sub_location_code, measure_date) |>
  summarise(nonregistered_ba = sum(nonregistered_ba), 
            registered_ba = sum(registered_ba),
            total_ba = nonregistered_ba + registered_ba)




eicmethods::raw_submission_data() |> 
  filter(measure_id == "EST") |>
  rename("nonregistered_est" = user_data_1, "registered_est" = user_data_2) |>
  select(-contains("user_data"), -measure_id) |>
  mutate(
    nonregistered_est = as.numeric(nonregistered_est),
    registered_est = as.numeric(registered_est),
    overall_est = nonregistered_est+registered_est,
    hours = overall_est*37.5)



eicmethods::raw_submission_data() |> 
  filter(measure_id %in% c("SSUEO")) |>  
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
  pivot_longer(contains("registered")) |> 
  ggplot(aes(measure_date, value, colour = name)) +
  geom_line() + 
  facet_wrap(~health_board_name)
  



## By type ---------------------------------------------------------------




# Training ----------------------------------------------------------------




eicmethods::raw_submission_data() |> 
  filter(measure_id %in% c("PLE1"),
         sub_location_code == "S08000015_49") |> 
  mutate(cum_sum = cumsum(as.numeric(user_data_2))/3,
         .before = user_data_3) |> 
  ggplot(aes(measure_date, cum_sum)) + 
  geom_line() + 
  theme(axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.minor.ticks.y.left = element_blank()
  ) +
  theme_minimal()
