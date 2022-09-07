library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)


add_deployment_id = function(df) {
  avg_date = mean(df$start_time_lst, na.rm = TRUE)
  rows = which(avg_date >= camml_deployments_clean$ambient_sampling_start_date &
                 avg_date <= camml_deployments_clean$ambient_sampling_end_date)
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

  df_wide = read_csv(fn, show_col_types = FALSE)
  df_long = pivot_longer(df_wide, cols = c(-contains("Time_")),
                         names_to = c("measurement", "units"),
                         names_pattern = "^([^_]+)_\\[(.+)\\]$") |>
    clean_names() %>%
    mutate(start_time_lst = coalesce(mdy_hm(start_time_lst, tz = "MST"),
                                     mdy_hms(start_time_lst, tz = "MST")),
           end_time_lst = coalesce(mdy_hm(end_time_lst, tz = "MST"),
                                   mdy_hms(end_time_lst, tz = "MST"))) |>
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
write_csv(all_other_data2, "parsed_data/camml/camml_other_data.csv")
