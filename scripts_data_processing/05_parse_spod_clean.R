library(tidyverse)
library(here)
library(lubridate)
library(janitor)
source(here("scripts_spod_processing", "useful_spod_functions.R"))

# There are 5 co-located CAMML/SPOD deployments
# 1. Great Western - Ivey
# 2. Cub Creek - Knight
# 3. Eagle Pointe
# 4. Civitas - LoneTree
# 5. Vetting Pad - Bella Romero

# Getting Data ----

### Great Western - Ivey
# Downloaded from https://cohealthviz.dphe.state.co.us/t/EnvironmentalEpidemiologyPublic/views/Ivey/DataDetails?%3AshowAppBanner=false&%3Adisplay_count=n&%3AshowVizHome=n&%3Aorigin=viz_share_link&%3AisGuestRedirectFromVizportal=y&%3Aembed=y

### Cub Creek - Knight
# Downloaded from https://cohealthviz.dphe.state.co.us/t/EnvironmentalEpidemiologyPublic/views/Knight/DataDetails?%3AshowAppBanner=false&%3Adisplay_count=n&%3AshowVizHome=n&%3Aorigin=viz_share_link&%3AisGuestRedirectFromVizportal=y&%3Aembed=y

### Eagle Pointe
spod_files = readRDS("~/r-analysis/spod.surveillance/data/sensitconnect/file_info_table.RDS")
eagle_pointe_files = spod_files |>
  filter(spod_id == "1033") |>
  filter(no_points > 0) |>
  filter(start_time >= (mdy_hms("5/14/2021 22:57:00") - days(1)),
         start_time <= (mdy_hms("7/17/2021 17:46:13") + days(1))) |>
  pull(filename) %>%
  file.path("~/r-analysis/spod.surveillance", .)

read_sensitconnect_folder_data = function(fn) {
  read_csv(fn, col_types = cols(
  `_id` = col_character(),
  id = col_character(),
  date = col_datetime(format = ""),
  pid1_PPB_Calc = col_double(),
  pid1_mvRaw = col_double(),
  temp = col_double(),
  rh_Humd = col_double(),
  pressure_mbar = col_double(),
  ws_speed = col_double(),
  ws_direction = col_double(),
  tc_temp = col_double(),
  tc_heatOutput = col_double(),
  tc_setPoint = col_double(),
  batt_voltage = col_double(),
  chrg_current = col_double(),
  run_current = col_double(),
  lat = col_double(),
  long = col_double(),
)) |>
    select(id, `_id`, date, lat, long, pid1_PPB_Calc, temp, rh_Humd, ws_speed, ws_direction)
}

eagle_pointe_data = map_dfr(eagle_pointe_files, read_sensitconnect_folder_data)

eagle_pointe_data |>
  ggplot(aes(x = date, y = pid1_PPB_Calc)) +
  geom_point()

write_csv(eagle_pointe_data, "data_spod_overlap/EaglePointe_PIDdata.csv")

### Civitas - LoneTree
# Downloaded from AirSense

### Bella
# Downloaded from https://cohealthviz.dphe.state.co.us/t/EnvironmentalEpidemiologyPublic/views/Bella_datadash_beta_english/DataDetails?%3Adisplay_count=n&%3Aembed=y&%3AisGuestRedirectFromVizportal=y&%3Aorigin=viz_share_link&%3AshowAppBanner=false&%3AshowVizHome=n


## Making a unified and aligned dataset ----

### Unifying

spod_ivey_raw = read_csv("data_spod_overlap/Ivey_PIDdata_20210310.csv")
spod_cubcreek_raw = read_csv("data_spod_overlap/Knight_PIDdata.csv")
spod_eagle_raw = read_csv("data_spod_overlap/EaglePointe_PIDdata.csv")
spod_lonetree_raw = read_csv("data_spod_overlap/LoneTree_PIDdata_AirSense.csv")
spod_bella_raw = read_csv("data_spod_overlap/Bella_PID_data.csv")

spod_ivey = spod_ivey_raw |>
  transmute(start_time = ymd_hms(start),
            tvoc_ppb = TVOCs,
            pid_type = PIDtype,
            data_type = "final",
            correct_loc = TRUE,
            pad_name = "Ivey")

spod_cubcreek = spod_cubcreek_raw |>
  transmute(start_time = start,
            tvoc_ppb = TVOCs,
            pid_type = "SPOD",
            data_type = "final",
            correct_loc = TRUE,
            pad_name = "Knight")

spod_bella = spod_bella_raw |>
  filter(PIDtype == "SPOD") |>
  transmute(start_time = start,
            tvoc_ppb = TVOCs,
            pid_type = PIDtype,
            data_type = "final",
            correct_loc = TRUE,
            pad_name = "Vetting") |>
  group_by(start_time) |>
  slice_head(n = 1)

# We need to clean up the Eagle and LoneTree data (make sure they're properly aligned)

camml_data = read_csv("parsed_data/camml/camml_voc_data_gcid.csv")
camml_eagle = camml_data |>
  filter(pad_name == "Eagle Pointe (Suncor)")
camml_eagle_range = c(min(camml_eagle$start_time_lst), max(camml_eagle$end_time_lst))
spod_eagle = spod_eagle_raw |>
  filter(date >= camml_eagle_range[[1]],
         date <= camml_eagle_range[[2]]) |>
  transmute(start_time = date,
            tvoc_ppb = pid1_PPB_Calc,
            spod_temp = temp,
            spod_rh = rh_Humd,
            pid_type = "SPOD",
            data_type = "raw",
            correct_loc = lat != 0,
            pad_name = "Eagle Pointe (Suncor)")

# we don't have CAMML Lone Tree data yet - so there's not really much point in cleaning up that
# data yet. So let's not worry about it and just unify.

spod_unified =
  bind_rows(
    spod_ivey,
    spod_cubcreek,
    spod_eagle,
    spod_bella
)

### Aligning

camml_gcid = read_csv("parsed_data/camml/camml_voc_data_gcid.csv") |>
  select(start_time_lst, end_time_lst, deployment_name, pad_name, gc_id, deployment_id) |>
  distinct_all()

spod_gcid = left_join(spod_unified, camml_gcid,
                      by = join_by(start_time >= start_time_lst,
                                   start_time < end_time_lst,
                                   pad_name == pad_name)) |>
  arrange(start_time)

# Check that my alignment has mostly worked out
spod_gcid |>
  ggplot(aes(x = start_time, y= gc_id, color = pad_name)) +
  geom_point()
spod_gcid |>
  filter(pad_name == "Vetting",
         is.finite(gc_id)) |>
  group_by(deployment_name) |>
  summarize(start = min(start_time),
            end = max(start_time),
            n = n())
# Oh right, this is missing some of the data that's on the dashboard because we're limiting it to just
# SPOD measurements (not the CAMML PID)

# spod_gcid_overlap = spod_gcid |>
#   filter(is.finite(gc_id))
write_csv(spod_gcid, "parsed_data/spod_data_gcid.csv")

#
# spod_gcid_avg = spod_gcid_overlap |>
#   group_by(start_time_lst, end_time_lst, pad_name, deployment_name, gc_id) |>
#   summarize(sd_tvoc = sd(tvoc_ppb, na.rm = TRUE),
#             tvoc_ppb = mean(tvoc_ppb, na.rm = TRUE),
#             data_type = data_type[[1]],
#             correct_loc = all(correct_loc),
#             spod_temp = mean(spod_temp, na.rm = TRUE),
#             spod_rh = mean(spod_rh, na.rm = TRUE),
#             .groups = "drop")
# spod_gcid_bucket = spod_gcid_overlap |>
#   select(start_time, tvoc_ppb, start_time_lst, end_time_lst, gc_id) |>
#   mutate(gc_length = as.numeric(end_time_lst - start_time_lst),
#          bucket = floor(as.numeric(start_time - start_time_lst)/60/(gc_length/9))+1) |>
#   summarize(tvoc_ppb = mean(tvoc_ppb, na.rm = TRUE),
#             .by = c("gc_id", "bucket")) |>
#   pivot_wider(names_from = bucket, values_from = tvoc_ppb, names_prefix = "tvoc_ppb_")
#
# spod_gcid_clean = left_join(spod_gcid_avg, spod_gcid_bucket, by = "gc_id")
#
# write_csv(spod_gcid_clean, "parsed_data/spod_overlap_data_gcid_clean.csv")
#
#
#
#
#
