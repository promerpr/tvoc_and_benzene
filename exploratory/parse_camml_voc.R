library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)

df_wide = read_csv("data_camml/CCND_EaglePointe_MR_r1 - Sheet1.csv")
pivot_longer(df_wide, cols = c(-contains("Time_")))
# Ok, I have 3 pieces of information that I want to get from 2 columns:
# Name of VOC, Units of VOC
# Amount of VOC,
# Flags in the measurement

# Ok. Pivotting twice was straightforward and solved my problem without trying to do it
# all in one go using spec or something.
df_long = pivot_longer(df_wide, cols = c(-contains("Time_")),
             names_to = c("VOC", ".value"),
             names_sep = "_") %>%
  pivot_longer(cols = starts_with("["),
               names_to = "units",
               values_to = "concentration",
               names_transform = \(x) str_remove_all(x, "[\\[\\]]")) %>%
  clean_names() %>%
  mutate(start_time_lst = mdy_hms(start_time_lst, tz = "MST"),
         end_time_lst = mdy_hms(end_time_lst, tz = "MST"))

ggplot(df_long, aes(x = start_time_lst, y = concentration, color = voc), show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  scale_y_log10()

# Ok, that's reasonable.
# I think the next steps are:
# 1. Pull in information about the deployments
# 2. Parse the other

read_xlsx("data_camml/CAMML_Deployment_Summary.xlsx")
# Ok, I can't get the hyperlinks out directly, but I can take advantage of the fact that the CAMML
# is only at one place at once.
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


tail(camml_deployments_clean)

add_deployment_id = function(df) {
  avg_date = mean(df$start_time_lst, na.rm = TRUE)
  rows = which(avg_date >= camml_deployments_clean$ambient_sampling_start_date &
                             avg_date <= camml_deployments_clean$ambient_sampling_end_date)
  if (length(rows) != 1) {
    stop("Multiple deployments identified (somehow)")
  }
  df$deployment_id = camml_deployments_clean$deployment_id[rows]
  df
}

deployment_info = camml_deployments_clean |>
  select(pad_name = pad_name_sampling_log, pad_lat, pad_lon, camml_lat, camml_lon, operational_phase,
         distance_to_source_ft, source_direction_degrees)
df_long |>
  add_deployment_id() |>
  left_join(camml_deployments_clean, by = "deployment_id")


