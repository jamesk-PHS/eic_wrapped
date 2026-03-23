





library(tidyverse)
library(phsmethods)
library(openxlsx2)

data <- arrow::read_parquet("data/Overview TDE - data table (4).parquet") |> 
  janitor::clean_names() |> 
  as_tibble()




data |> 
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




# New teams in financial year ---------------------------------------------


map_df(list.files("/conf/EIC/Data Submission/Reference Files/R Process/HB Reference Files/1. Latest Files", 
                  full.names = TRUE), function(x){
                    
                    health_board <- base::basename(x)
                    health_board <- str_sub(health_board, 1, -21)
                      
                   data <- openxlsx2::read_xlsx(x, sheet = "Current Reference File") |> 
                      mutate(across(everything(), ~as.character(.x)),
                             health_board = health_board)
                    
                    return(data)
                    
                    }) |> 
  as_tibble() |> 
  mutate(open_date = ymd(open_date),
         fin_year = phsmethods::extract_fin_year(open_date),
         year = str_sub(fin_year, 1, 4),
         year = as.numeric(year)) |> 
  filter(year == max(year) | year == max(year)-1) |> 
  group_by(year, health_board) |> 
  summarise(n = n())










rbind(arrow::read_parquet("/conf/EIC/Data Submission/Submission Reports/Revised reports/data/outputs/20260203_submission_report_data_monthly.parquet"),
      arrow::read_parquet("/conf/EIC/Data Submission/Submission Reports/Revised reports/data/outputs/20260203_submission_report_data_partial_measures.parquet")
) |> 
  filter(hb_name == "Ayrshire and Arran",
         year(month) == "2024") |> 
  summarise(n_submitted = sum(n_submitted))


rbind(arrow::read_parquet("/conf/EIC/Data Submission/Submission Reports/Revised reports/data/outputs/20260203_submission_report_data_team_level.parquet"),
      arrow::read_parquet("/conf/EIC/Data Submission/Submission Reports/Revised reports/data/outputs/20260203_partial_measure_team_level_data.parquet")
) |> 
  filter(hb_name == "Ayrshire and Arran",
         year(month) == "2024") |> 
  group_by(str_to_title(sub_location_name)) |> 
  summarise(n_submitted = sum(n_submitted))



