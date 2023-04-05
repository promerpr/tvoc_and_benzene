library(tidyverse)
library(here)
library(lubridate)
library(janitor)
source(here("scripts_spod_processing", "useful_spod_functions.R"))

combined_f = function(fn) {
  if (endsWith(fn, "txt")) {
    df = read_tsv(fn, col_names = FALSE, guess_max = 1e7)
  } else {
    df = read_csv(fn, col_names = FALSE, guess_max = 1e7)
    if (ncol(df) == 1) {
      df = read_tsv(fn, col_names = FALSE, guess_max = 1e7)
    }
  }
  df = df |>
    fix_spod_colnames() |>
    clean_up_raw_spod() |>
    downsample_spod()
}
# petro_bef_files =
#   list.files("X:/Shared drives/CDPHE OGHIR/Total VOCs Monitoring/SPOD Data/1160 - Petro BEF Pad - @ Park/",
#              pattern = "^[0-9]+[.]TXT$",
#              full.names = TRUE)
#
# petro_bef_data = map_dfr(petro_bef_files[1:10], combined_f)
combined_f("~/Desktop/test_spod_file.TXT")

fn = ivey_files[47]
df = read_tsv(fn, col_names = FALSE, guess_max = 1e7)
df2 = df |>
  fix_spod_colnames() |>
  clean_up_raw_spod()
df2 |>
  filter(DATE <= ymd_hms("2021-01-23 00:05:00")) |>
  reframe(across(everything(), munique, na.rm = TRUE))
