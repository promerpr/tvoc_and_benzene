library(tidyverse)

camml_other_wide = read_csv("parsed_data/camml/camml_other_data_wide_gcid.csv")
camml_voc = read_csv("parsed_data/camml/camml_voc_data_gcid.csv")

camml_gc_runs = camml_voc |>
  select(start_time_lst, end_time_lst, gc_id) |>
  distinct_all()
camml_other_aligned = camml_other_wide |>
  mutate(math_winddir_rad = 2*pi * (270 - wind_direction_degrees)/360,
         u = wind_speed_ms * cos(math_winddir_rad),
         v = wind_speed_ms * sin(math_winddir_rad)) |>
  select(-wind_speed_ms, -wind_direction_degrees, -math_winddir_rad) |>
  filter(!is.na(gc_id)) |>
  group_by(gc_id, deployment_id, deployment_name, pad_name) |>
  summarize(n = sum(is.finite(tvoc_pid_ppbv)),
            across(rh_percent:v, mean, na.rm = TRUE),
            tvoc_pid_q05_ppbv = quantile(tvoc_pid_ppbv, 0.05, na.rm = TRUE),
            tvoc_pid_q95_ppbv = quantile(tvoc_pid_ppbv, 0.95, na.rm = TRUE),
            .groups = "drop")

camml_other_aligned2 = camml_other_aligned |>
  mutate(WindSpeed = sqrt(u^2 + v^2),
         WindDirection = (atan2(v, u) * 360/(2*pi)) %% 360) |>
  select(-u, -v) %>%
  right_join(camml_gc_runs, ., by = "gc_id")



spod_data_gcid = read_csv("parsed_data/spod_data_gcid.csv")
spod_data_aligned = spod_data_gcid |>
  filter(!is.na(gc_id)) |>
  group_by(gc_id, deployment_id, deployment_name, pad_name) |>
  summarize(spod_tvoc_sd = sd(tvoc_ppb, na.rm = TRUE),
            spod_tvoc_ppb = mean(tvoc_ppb, na.rm = TRUE),
            data_type = data_type[[1]],
            correct_loc = all(correct_loc),
            spod_temp = mean(spod_temp, na.rm = TRUE),
            spod_rh = mean(spod_rh, na.rm = TRUE),
            .groups = "drop")

camml_benzene = camml_voc |>
  filter(voc == "benzene") |>
  pivot_wider(names_from = voc, values_from = concentration)

combined_data =
  inner_join(camml_benzene, select(camml_other_aligned2, -start_time_lst, -end_time_lst),
             by= c("gc_id", "deployment_id", "deployment_name", "pad_name")) |>
  inner_join(spod_data_aligned,
             by= c("gc_id", "deployment_id", "deployment_name", "pad_name"))

spod_gcid_bucket = spod_data_gcid |>
  filter(!is.na(gc_id)) |>
  select(start_time, tvoc_ppb, start_time_lst, end_time_lst, gc_id) |>
  mutate(gc_length = as.numeric(end_time_lst - start_time_lst),
         bucket = floor(as.numeric(start_time - start_time_lst)/60/(gc_length/9))+1) |>
  summarize(tvoc_ppb = mean(tvoc_ppb, na.rm = TRUE),
            .by = c("gc_id", "bucket")) |>
  pivot_wider(names_from = bucket, values_from = tvoc_ppb, names_prefix = "tvoc_ppb_")

wind_gcid_bucket = camml_other_wide |>
  filter(!is.na(gc_id)) |>
  rename(start_time = start_time_lst) |>
  select(-end_time_lst) |>
  left_join(camml_gc_runs, by = "gc_id") |>
  mutate(gc_length = as.numeric(end_time_lst - start_time_lst),
         bucket = floor(as.numeric(start_time - start_time_lst)/60/(gc_length/9))+1) |>
  mutate(math_winddir_rad = 2*pi * (270 - wind_direction_degrees)/360,
         u = wind_speed_ms * cos(math_winddir_rad),
         v = wind_speed_ms * sin(math_winddir_rad)) |>
  summarize(u = mean(u, na.rm = TRUE),
            v = mean(v, na.rm = TRUE),
            .by = c("gc_id", "bucket")) |>
  mutate(wind_speed = sqrt(u^2 + v^2),
         wind_direction = (atan2(v, u) * 360/(2*pi)) %% 360,
         .keep = "unused") |>
  pivot_wider(names_from = bucket, values_from = c(wind_speed, wind_direction))

wind_spod_buckets = full_join(spod_gcid_bucket, wind_gcid_bucket, by = "gc_id") |>
  arrange(gc_id) |>
  semi_join(combined_data, by = "gc_id")

write_csv(combined_data, "parsed_data/combined_camml_spod_data.csv")
write_csv(wind_spod_buckets, "parsed_data/wind_spod_buckets.csv")
