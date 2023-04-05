library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)


add_deployment_id = function(df) {
  avg_date = mean(df$start_time_lst, na.rm = TRUE)
  rows = which(avg_date >= camml_deployments_clean$ambient_sampling_start_date &
                 avg_date <= camml_deployments_clean$ambient_sampling_end_date)
  print(rows)
  if (length(rows) > 1) {
    # Manual patching for 14 and 15
    if (identical(rows, c(14L, 15L))) {
      if (quantile(df$start_time_lst, 0.05, na.rm = TRUE) < ymd("2019-07-29")) {
        rows = 14
      } else {
        rows = 15
      }
    } else {
      browser()
      stop("Multiple deployments identified (somehow)")
    }
  } else if (length(rows) == 0) {
    browser()
    stop("No deployments identified")
  }
  df$deployment_id = camml_deployments_clean$deployment_id[rows]
  df
}

read_other = function(fn) {

  df_wide = read_csv(fn, show_col_types = FALSE) |>
    mutate(file_name = fn)
  df_long = pivot_longer(df_wide, cols = c(-contains("Time_"), -file_name),
                         names_to = c("measurement", "units"),
                         names_pattern = "^([^_]+)_\\[(.+)\\]$") |>
    clean_names()

  start_has_seconds = grepl(":.+:", df_long$start_time_lst[1])
  end_has_seconds = grepl(":.+:", df_long$end_time_lst[1])
  if (start_has_seconds) {
    f_start = mdy_hms
  } else {
    f_start = mdy_hm
  }
  if (end_has_seconds) {
    f_end = mdy_hms
  } else {
    f_end = mdy_hm
  }
  df_long = df_long |>
    mutate(start_time_lst = f_start(start_time_lst, tz = "MST"),
           end_time_lst = f_end(end_time_lst, tz = "MST")) |>
    add_deployment_id()
  df_long
}

wanted_files = list.files(path = "data_camml/", pattern = "_OtherData_", full.names = TRUE)

camml_deployments_clean = read_csv("parsed_data/camml/camml_deployments_clean.csv", show_col_types = FALSE)
camml_deployment_info = read_csv("parsed_data/camml/camml_deployment_info.csv", show_col_types = FALSE)
all_other_data = map_dfr(wanted_files, read_other)

all_other_data |>
  count(deployment_id, measurement) |>
  pivot_wider(names_from = measurement, values_from = n) |>
  print(width = Inf, n = Inf)

all_other_data2 = left_join(all_other_data, camml_deployment_info, by = "deployment_id")

# I think it's fair to say that we actually want the data in wide format.
# So let's do that.

all_other_data_units_aligned = all_other_data2 |>
  mutate(value = case_when(measurement == "O3" & units == "ppmV" ~ value * 1000,
                           measurement == "VOC(pid)" & units == "IBeq_ppbV" ~ value,
                           TRUE ~ value),
         units = case_when(measurement == "O3" & units == "ppmV" ~ "ppbV",
                           measurement == "VOC(pid)" & units == "IBeq_ppbV" ~ "ppbV",
                           TRUE ~ units))

all_other_data_units_aligned |>
  count(measurement, units)

# I think there's an issue with the units for TVOC in deployment 9:
all_other_data_units_aligned |>
  filter(measurement == "VOC(pid)") |>
  group_by(deployment_id) |>
  summarize(sd = sd(value, na.rm = TRUE))
# That's pretty clear - sd for deployment 9 is 1000x smaller than any other deployment

all_other_data_units_corrected = all_other_data_units_aligned |>
  mutate(value = ifelse(deployment_id == 9 & measurement == "VOC(pid)",
                        value * 1000,
                        value))

all_other_data_units_corrected |>
  filter(measurement == "VOC(pid)") |>
  group_by(deployment_id) |>
  summarize(sd = sd(value, na.rm = TRUE))

# Next, pivot the data wider

all_other_data_wide = all_other_data_units_corrected |>
  pivot_wider(names_from = c(measurement, units), values_from = value) |>
  clean_names() |>
  rename(nox_ppbv = n_ox_ppb_v,
         wind_speed_ms = wind_speed_m_s,
         p_barometric_mmhg = p_barometric_mm_hg,
         solar_radiation_wm2 = solar_radiation_w_m2,
         ch4_ppmv = ch4_ppm_v,
         pm25_ugm3 = pm2_5_ltp_ug_m3 ,
         pm10_ugm3  = pm10_ltp_ug_m3,
         no_ppbv = no_ppb_v,
         no2_ppbv = no2_ppb_v,
         o3_ppbv = o3_ppb_v,
         tvoc_pid_ppbv = voc_pid_ppb_v ,
         co2_ppmv = co2_ppm_v) |>
  arrange(deployment_id)

all_other_data_wide |>
  group_by(start_time_lst) |>
  filter(n() > 1) |>
  ungroup() |>
  count(deployment_id)
# Ok, I think that's correct - we're seeing a slight overlap between 12 & 13 and a larger
# overlap between 14 and 15

ggplot(all_other_data_wide, aes(x = start_time_lst, y= deployment_id)) +
  geom_point()

# write_csv(all_other_data_units_corrected, "parsed_data/camml/camml_other_data_long.csv")
write_csv(all_other_data_wide, "parsed_data/camml/camml_other_data_wide.csv")
