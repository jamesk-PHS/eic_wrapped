

data |> 
  mutate(across(9:11, as.numeric)) |> 
  filter(!is.na(calc_rate)) |> 
  group_by(hb_name, sub_location_code, measure_id) |> 
  mutate(change = calc_rate - lag(calc_rate),
         sign = if_else(sign(change) == 1, TRUE, FALSE)) |>
  summarise(nrow = max(row_number()),
            sum_sign = sum(sign, na.rm = T)) |>
  group_by(hb_name, sub_location_code) |> 
  filter(sum_sign == max(sum_sign)) |>  View()
  


data |> 
  mutate(across(9:11, as.numeric)) |> 
  filter(!is.na(calc_rate)) |> 
  group_by(hb_name, sub_location_code, measure_id) |> 
  mutate(change = calc_rate - lag(calc_rate),
         sign = if_else(sign(change) == 1, TRUE, FALSE)) |> 
  group_by(hb_name, sub_location_code, measure_id, sign) |> 
  mutate(cum_sign = cumsum(sign)) |>  View()


data |> 
  mutate(across(9:11, as.numeric)) |> 
  filter(!is.na(calc_rate)) |> 
  group_by(hb_name, sub_location_name) |> 
  mutate(change = calc_rate - lag(calc_rate)) |> 
  summarise(maintain = sum(if_else(change == 0, TRUE, FALSE), na.rm = T))  |>
  filter(str_detect(hb_name, "ARRAN")) |> 
  ggplot(aes(label = sub_location_name, size = maintain, colour = maintain)) + 
  ggwordcloud::geom_text_wordcloud(show.legend = F) +
  theme_minimal() +
  facet_wrap(~hb_name)
