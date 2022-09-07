library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)

df_wide = read_csv("data_camml/CCND_EaglePointe_OtherData_r1 - 1 minute.csv")

# Does the double pivot work?
pivot_longer(df_wide, cols = c(-contains("Time_")),
             names_to = c("VOC", ".value"),
             names_sep = "_") %>%
  pivot_longer(cols = starts_with("["),
               names_to = "units",
               values_to = "concentration",
               names_transform = \(x) str_remove_all(x, "[\\[\\]]")) %>%
  clean_names()

# No, it doesn't.
# Probably better to do it as one go
pivot_longer(df_wide, cols = c(-contains("Time_")),
             names_to = c("VOC", "units"),
             names_pattern = "^([^_]+)_\\[(.+)\\]$") |>
  clean_names() %>%
  mutate(start_time_lst = coalesce(mdy_hm(start_time_lst, tz = "MST"),
                                   mdy_hms(start_time_lst, tz = "MST")),
         end_time_lst = coalesce(mdy_hm(end_time_lst, tz = "MST"),
                                 mdy_hms(end_time_lst, tz = "MST")))
