
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(readr)

# Read in CAIR raw data
raw <- read.csv("/conf/EIC/Data Submission/Submission Reports/Revised reports/data/submission_extracts/1-sub_reports_raw_extract.csv", 
               fileEncoding = "UTF-16LE",
               sep = "\t", header = T) |> 
  as_tibble() |> 
  select(-X) |>
  janitor::clean_names() |> 
  mutate(measure_date = dmy(measure_date),
         financial_year = phsmethods::extract_fin_year(measure_date))

# Pull PTA data into one file
# Some aggregations are required for areas with multiple rosters attached
pta <- raw |>
  filter(measure_id %in% c("ALR", "NOABS", "OAR", "MLR", "SAR", "SLR")) |>
  rename(skill_mix = user_data_1, measure_id2 = user_data_2,
         week_count = user_data_3, av_in_post_wte = user_data_4,
         hours_lost = user_data_5, hours_lost_wte = user_data_6) |>
  select(-contains("user_data")) |>
  mutate(across(9:12, as.numeric),
         skill_mix = str_to_upper(skill_mix)) |> 
  group_by(health_board_name, financial_year, sub_location_code, measure_date, skill_mix, week_count) |>
  summarise(av_in_post_wte = sum(av_in_post_wte), hours_lost = sum(hours_lost), hours_lost_wte = sum(hours_lost_wte))

# Taking distinct columns. Normally this will identify the average contracted
# hours, but if a team has multiple rosters but only some leave types in one,
# they won't sum correctly as some measures will be excluded from sum.

# Taking the max value is the valid solution.
# We don't need to do the above for new data, only old process data (submitted at roster level).
av_in_post_wte <- pta |>
  distinct(health_board_name, sub_location_code, measure_date, skill_mix, av_in_post_wte) |>
  group_by(health_board_name, sub_location_code, measure_date, skill_mix) |>
  summarise(av_in_post_wte = max(av_in_post_wte)) |>
  # Some data cleaning and prep - wider format for easier calculations
  mutate(skill_mix = case_when(
    skill_mix == "NON REGISTERED" ~ "nonregistered_ssts",
    skill_mix == "REGISTERED" ~ "registered_ssts"
  )) |>
  pivot_wider(names_from = skill_mix, values_from = av_in_post_wte, values_fill = 0) |>
  mutate(overall_ssts = nonregistered_ssts+registered_ssts)

# Getting the aggregated number of hours lost per team/month, any abs type
pta_hours_lost_overall <- pta |>
  group_by(sub_location_code, measure_date, skill_mix, week_count) |>
  summarise(hours_lost = sum(hours_lost)) |>
  mutate(skill_mix = case_when(
    skill_mix == "NON REGISTERED" ~ "nonregistered_abs",
    skill_mix == "REGISTERED" ~ "registered_abs"
  )) |>
  pivot_wider(names_from = skill_mix, values_from = hours_lost) |>
  replace_na(list(nonregistered_abs=0, registered_abs=0)) |>
  mutate(overall_abs = nonregistered_abs+registered_abs)

# matching contracted hours onto absences
pta_measure <- full_join(pta_hours_lost_overall, av_in_post_wte)
# Converting WTE to hours
pta_measure <- pta_measure |>
  mutate(hours_in_one_wte = case_when(
    measure_date < dmy("01/04/2024") ~ 37.5,
    measure_date < dmy("01/04/2026") ~ 37,
    TRUE ~ 36 # Adding in the future change for resilience
  ),
  nonregistered_ssts = nonregistered_ssts*week_count*hours_in_one_wte,
  registered_ssts = registered_ssts*week_count*hours_in_one_wte,
  overall_ssts = overall_ssts*week_count*hours_in_one_wte,
  
  nonregistered_pta = nonregistered_abs/nonregistered_ssts,
  registered_pta = registered_abs/registered_ssts,
  overall_pta = overall_abs/overall_ssts)

# Overall measure value
pta_overall <- pta_measure |>
  mutate(measure_id = "PTA") |>
  select(sub_location_code, measure_id, measure_date, measure_value = overall_pta)

# Getting funded establishment figures
est <- raw |>
  filter(measure_id == "EST") |>
  rename("nonregistered_est" = user_data_1, "registered_est" = user_data_2) |>
  select(-contains("user_data"), -measure_id) |>
  mutate(
    nonregistered_est = as.numeric(nonregistered_est),
    registered_est = as.numeric(registered_est),
    overall_est = nonregistered_est+registered_est)

# Calculating establishment variance
vac_measure <- full_join(est, av_in_post_wte)

# Lots of individual columns in this dataframe - simplified below
vac_measure <- vac_measure |>
  mutate(nonregistered_numerator = nonregistered_est-nonregistered_ssts,
         registered_numerator = registered_est-registered_ssts,
         overall_numerator = overall_est-overall_ssts,
         
         nonregistered_vac = nonregistered_numerator/nonregistered_est,
         registered_vac = registered_numerator/registered_est,
         overall_vac = overall_numerator/overall_est)

# Simple output with the measure value
vac_overall <- vac_measure |>
  mutate(measure_id = "VAC") |>
  select(sub_location_code, measure_id, measure_date, measure_value = overall_vac)

# Pulling date/wte/weeks lookup for SSU measures
wte_week_lookup <- pta_measure |>
  ungroup() |>
  distinct(measure_date, week_count, hours_in_one_wte)

# SSUBA
# Aggregating the separate submissions
ssuba_measure <- raw |>
  filter(measure_id %in% c("SSUA", "SSUB")) |>
  rename(nonregistered_ba = user_data_1, registered_ba = user_data_2) |>
  mutate(across(.cols = c("nonregistered_ba", "registered_ba"), ~as.numeric(.x))) |>
  group_by(sub_location_code, measure_date) |>
  summarise(nonregistered_ba=sum(nonregistered_ba), registered_ba=sum(registered_ba)) |>
  # Joining EST and lookup for calculations
  left_join(est |> select(-c(health_board_code_9_curr, health_board_name, location_code))) |>
  left_join(wte_week_lookup) |>
  # convert hours to WTE
  mutate(nonregistered_ba_wte = nonregistered_ba/week_count/hours_in_one_wte,
         registered_ba_wte = registered_ba/week_count/hours_in_one_wte,
         overall_ba_wte = nonregistered_ba_wte+registered_ba_wte,
         
         # calc measure
         nonregistered_ssuba = nonregistered_ba_wte/nonregistered_est,
         registered_ssuba = registered_ba_wte/registered_est,
         overall_ssuba = overall_ba_wte/overall_est)

ssuba_overall = ssuba_measure |>
  mutate(measure_id = "SSUBA") |>
  distinct(sub_location_code, measure_id, measure_date, measure_value = overall_ssuba)

# SSUEO
ssueo_measure <- raw |>
  filter(measure_id == "SSUEO") |>
  rename(skill_mix = user_data_1, week_count = user_data_2, excess = user_data_3, overtime = user_data_4) |>
  mutate(across(.cols = c("excess", "overtime"), ~as.numeric(.x)),
         excessovertime = excess+overtime,
         skill_mix = str_to_upper(skill_mix)) |>
  
  # Just like PTA, we sometimes have to aggregate
  group_by(sub_location_code, measure_date, skill_mix) |>
  summarise(excessovertime = sum(excessovertime)) |>
  mutate(skill_mix = case_when(
    skill_mix == "NON REGISTERED" ~ "nonregistered_eo",
    skill_mix == "REGISTERED" ~ "registered_eo"
  )) |>
  pivot_wider(names_from = skill_mix, values_from = excessovertime) |>
  replace_na(list(nonregistered_eo=0, registered_eo=0)) |>
  
  # joining EST and lookup for calculations
  left_join(est |> select(-c(health_board_code_9_curr, health_board_name, location_code))) |>
  left_join(wte_week_lookup) |>
  
  # convert to WTE
  mutate(nonregistered_eo_wte = nonregistered_eo/week_count/hours_in_one_wte,
         registered_eo_wte = registered_eo/week_count/hours_in_one_wte,
         overall_eo_wte = nonregistered_eo_wte+registered_eo_wte,
         
         # calc measure
         nonregistered_ssueo = nonregistered_eo_wte/nonregistered_est,
         registered_ssueo = registered_eo_wte/registered_est,
         overall_ssueo = overall_eo_wte/overall_est)

ssueo_overall = ssueo_measure |>
  mutate(measure_id = "SSUEO") |>
  distinct(sub_location_code, measure_id, measure_date, measure_value = overall_ssueo)


# Compile a dataframe with calculated measures
overall_measure_dataframe = do.call(rbind, list(pta_overall, vac_overall, ssuba_overall, ssueo_overall)) |>
  mutate(measure_value_pretty = scales::percent(measure_value))
