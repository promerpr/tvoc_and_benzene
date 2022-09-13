# I think the truth is that this needs to be it's own file, with it's separate analysis.

library(tidyverse)
library(lubridate)
camml_other_raw = read_csv("parsed_data/camml/camml_other_data_gcid.csv")

# Clean up, pivot wider


camml_other = camml_other_raw |>
  mutate(value = case_when(measurement == "O3" & units == "ppmV" ~ value * 1000,
                           measurement == "VOC(pid)" & units == "IBeq_ppbV" ~ value,
                           TRUE ~ value),
         units = case_when(measurement == "O3" & units == "ppmV" ~ "ppbV",
                           measurement == "VOC(pid)" & units == "IBeq_ppbV" ~ "ppbV",
                           TRUE ~ units))


camml_other_wide = camml_other |>
  select(gc_id, gc_deployment_id, start_time_lst, end_time_lst, measurement, units, value) |>
  pivot_wider(id_cols = c(gc_id, gc_deployment_id, start_time_lst, end_time_lst),
              names_from = measurement,
              values_from = value)

# Now, figure out what's going on with gc_deployment_id
camml_other_wide |>
  count(gc_deployment_id) |>
  print(n = Inf)

camml_other_wide |>
  filter(is.na(gc_deployment_id)) |>
  ggplot(aes(x = start_time_lst, y = `VOC(pid)`*0 + 10)) +
  geom_jitter(width = 0) +
  geom_segment(data = camml_deployments_clean,
               aes(x = ambient_sampling_start_date, xend = ambient_sampling_end_date,
                   y = deployment_id, yend = deployment_id),
               color = "red", size = 2)

# Huh. Something seems weird here - those look like they should be assigned to a deployment, but aren't
camml_other_wide |>
  # filter(start_time_lst == ymd_hms("2017-03-07 20:35:00"))
  filter(floor_date(start_time_lst, "day") == ymd("2017-03-07")) |>
  print(n = 300)

camml_deployments_clean = read_csv("parsed_data/camml/camml_deployments_clean.csv", show_col_types = FALSE)
