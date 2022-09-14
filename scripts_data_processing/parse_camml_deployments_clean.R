library(tidyverse)
library(lubridate)
library(janitor)

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
         deployment_id = row_number()) |>
  rowwise() |>
  mutate(pad_name = pad_name_sampling_log,
         deployment_name = paste(company_name, pad_name, operational_phase, sep = "_")) |>
  ungroup()

deployment_info = camml_deployments_clean |>
  select(deployment_name, pad_name, pad_lat, pad_lon, camml_lat, camml_lon, operational_phase,
         distance_to_source_ft, source_direction_degrees, deployment_id)

write_csv(camml_deployments_clean, "parsed_data/camml/camml_deployments_clean.csv")
write_csv(deployment_info, "parsed_data/camml/camml_deployment_info.csv")

