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

read_deployment = function(fn) {
  df_wide = read_csv(fn, show_col_types = FALSE) |>
    mutate(file_name = fn)

  df_long = pivot_longer(df_wide, cols = c(-contains("Time_"), -file_name),
                         names_to = c("VOC", ".value"),
                         names_sep = "_") %>%
    pivot_longer(cols = starts_with("["),
                 names_to = "units",
                 values_to = "concentration",
                 names_transform = \(x) str_remove_all(x, "[\\[\\]]")) %>%
    clean_names() %>%
    mutate(start_time_lst = coalesce(mdy_hm(start_time_lst, tz = "MST"),
                                     mdy_hms(start_time_lst, tz = "MST")),
           end_time_lst = coalesce(mdy_hm(end_time_lst, tz = "MST"),
                                   mdy_hms(end_time_lst, tz = "MST")))
  add_deployment_id(df_long)
}

headers = read_csv("data_camml/CAMML_Deployment_Summary - Sampling Summary.csv", col_names = FALSE, n_max = 2,
                   show_col_types = FALSE) |>
  as.data.frame()

first_row = unlist(headers[1, ] )
headers[1, ] = vctrs::vec_fill_missing(first_row, "down")
colnames = map_chr(headers, \(x) paste0(x[!is.na(x)], collapse = "_"))
camml_deployments = read_csv("data_camml/CAMML_Deployment_Summary - Sampling Summary.csv",
                             col_names = colnames, show_col_types = FALSE, guess_max = 1e6,
                             skip = 2) |>
  clean_names()

camml_deployments_clean = camml_deployments |>
  tidyr::fill(year, .direction = "down") |>
  select(year:number_of_ambient_samples ) |>
  filter(!is.na(ambient_sampling_start_date)) |>
  mutate(ambient_sampling_start_date = coalesce(mdy_hm(ambient_sampling_start_date ),
                                                mdy_hms(ambient_sampling_start_date )),
         ambient_sampling_end_date = coalesce(mdy_hm(ambient_sampling_end_date),
                                              mdy_hms(ambient_sampling_end_date)),
         deployment_id = row_number())

wanted_files = list.files(path = "data_camml/", pattern = "_MR_", full.names = TRUE)
all_voc_data = map_dfr(wanted_files, read_deployment)

all_voc_data |>
  count(deployment_id)
tail(camml_deployments_clean) |>
  print(width = Inf)


deployment_info = camml_deployments_clean |>
  rowwise() |>
  mutate(pad_name = pad_name_sampling_log,
         deployment_name = paste(company_name, pad_name, operational_phase, sep = "_")) |>
  select(deployment_name, pad_name, pad_lat, pad_lon, camml_lat, camml_lon, operational_phase,
         distance_to_source_ft, source_direction_degrees, deployment_id)

combined_voc_data = left_join(all_voc_data, deployment_info, by = "deployment_id")

write_csv(combined_voc_data, "parsed_data/camml/camml_voc_data.csv")
benzene_data = combined_voc_data |>
  filter(voc == "benzene")
ggplot(benzene_data, aes(x = deployment_name, y = concentration)) +
  geom_violin() +
  coord_flip()
