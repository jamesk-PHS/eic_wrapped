tde_extract <- arrow::read_parquet("data/Overview TDE - data table (4).parquet") |> 
  janitor::clean_names() |> 
  as_tibble()


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

sub_board_options <- c("Please select", unique(eicmethods::submission_data("team")$hb_name))

NRV_board_options <- c("Please select", unique(tde_exract_expanded$hb_name))
