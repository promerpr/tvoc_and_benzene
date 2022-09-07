library(tidyverse)
library(rsample)
library(lubridate)

camml_voc = read_csv("parsed_data/camml/camml_voc_data_gcid.csv")
camml_other = read_csv("parsed_data/camml/camml_other_data_gcid.csv")

camml_other_units_fixed = camml_other |>
  mutate(value = case_when(measurement == "O3" & units == "ppmV" ~ value * 1000,
                           measurement == "VOC(pid)" & units == "IBeq_ppbV" ~ value,
                           TRUE ~ value),
         units = case_when(measurement == "O3" & units == "ppmV" ~ "ppbV",
                           measurement == "VOC(pid)" & units == "IBeq_ppbV" ~ "ppbV",
                           TRUE ~ units))

# CHeck that our units are correct
camml_other_units_fixed |>
  count(measurement, units)

# Check that our rows are uniquely identified
camml_other_units_fixed |>
  filter(measurement == "RH") |>
  group_by(gc_id, start_time_lst, end_time_lst, gc_start_time ) |>
  summarize(n = n(), .groups = "drop") |>
  filter(n > 1) |>
  count(floor_date(gc_start_time, "day")) |>
  print(n = Inf)

# Pivot Wider
camml_other_wide = camml_other_units_fixed |>
  select(gc_id, gc_deployment_id, start_time_lst, end_time_lst, measurement, units, value) |>
  pivot_wider(id_cols = c(gc_id, gc_deployment_id, start_time_lst, end_time_lst),
              names_from = measurement,
              values_from = value)

# Average down (doing the wind averaging correctly)
camml_other_aligned = camml_other_wide |>
  mutate(math_winddir_rad = 2*pi * (270 - WindDirection)/360,
         u = WindSpeed * cos(math_winddir_rad),
         v = WindSpeed * sin(math_winddir_rad)) |>
  select(-WindSpeed, -WindDirection, -math_winddir_rad) |>
  rename(tvoc = `VOC(pid)`) |>
  group_by(gc_id) |>
  summarize(n = sum(is.finite(tvoc)),
            across(RH:v, mean, na.rm = TRUE),
            tvoc_low = quantile(tvoc, 0.05, na.rm = TRUE),
            tvoc_high = quantile(tvoc, 0.95, na.rm = TRUE))
camml_other_aligned = camml_other_aligned |>
  mutate(WindSpeed = sqrt(u^2 + v^2),
         WindDirection = atan2(v, u))

# Now combine the voc and the other dataset
combined_camml_data = camml_voc |>
  filter(voc == "benzene") |>
  left_join(camml_other_aligned, by = "gc_id")

combined_camml_data |>
  count(n, sort = TRUE)

data_for_modeling = combined_camml_data |>
  filter(n >= 25) |>
  pivot_wider(names_from = voc, values_from = concentration)
data_for_modeling2 = data_for_modeling |>
  filter(!is.na(start_time_lst)) |>
  relocate(benzene, tvoc, .after = end_time_lst)

write_csv(data_for_modeling2, "parsed_data/camml/camml_combined_data.csv")

# I'd also like to make a "wide" version that I can use to attempt to look at TVOC and wind direction differently.
make_df = function(start_time, end_time, n) {
  data.frame(sub_id = seq_len(n),
             sub_start = seq(start_time, end_time, length.out = n+1)[1:n],
             sub_end = seq(start_time, end_time, length.out = n+1)[2:(n+1)])
}
sub_groups = camml_voc |>
  distinct(start_time_lst, end_time_lst, gc_id) |>
  na.omit() |>
  group_by(gc_id) |>
  summarize(make_df(start_time_lst, end_time_lst, n = 9),
            .groups = "drop")

q = sub_groups[1:90, ]
q
which()
o2 = camml_other_wide[17, ]
o2
ind = which(o2$gc_id == q$gc_id & o2$start_time_lst >= q$sub_start & o2$start_time_lst <= lead(q$sub_start,
                                                                                         default = ymd("2222-01-01")))
q[ind, ]
o2
# This will work, but how long will it take?


camml_other_wide$sub_id = NA
lead_sub_start =  lead(sub_groups$sub_start, n = 1,
                       default = now())
# sub_groups$lead_sub_start = lead_sub_start
for (i in seq_len(nrow(camml_other_wide))) {
  other_row = camml_other_wide[i, ]
  # match_group = sub_groups[sub_groups$gc_id == other_row$gc_id,]
  ind = which(other_row$gc_id == sub_groups$gc_id &
                other_row$start_time_lst >= sub_groups$sub_start &
                other_row$start_time_lst < lead_sub_start)
  if (length(ind) == 0) {
    next
  }
  camml_other_wide$sub_id[[i]] = sub_groups$sub_id[[ind]]
  if (i %% 10000 == 0) {
    message(i)
  }
}
camml_other_wide
camml_sub_aligned = camml_other_wide |>
  mutate(math_winddir_rad = 2*pi * (270 - WindDirection)/360,
         u = WindSpeed * cos(math_winddir_rad),
         v = WindSpeed * sin(math_winddir_rad)) |>
  select(-WindSpeed, -WindDirection, -math_winddir_rad) |>
  rename(tvoc = `VOC(pid)`) |>
  group_by(gc_id, sub_id) |>
  summarize(across(c(tvoc, u, v), mean, na.rm = TRUE),
            .groups = "drop") |>
  mutate(WindSpeed = sqrt(u^2 + v^2),
         WindDirection = atan2(v, u)) |>
  select(gc_id, sub_id, WindDirection, tvoc)
camml_sub_aligned |>
  na.omit()  |>
  tail()
camml_other_aligned |>
  filter(is.finite(tvoc), is.finite(WindDirection)) |>
  tail()
camml_wide_aligned = camml_sub_aligned |>
  na.omit() |>
  pivot_wider(names_from = sub_id, values_from = c(WindDirection, tvoc))
write_csv(camml_wide_aligned, "parsed_data/camml/camml_wide_spod_wind.csv")
