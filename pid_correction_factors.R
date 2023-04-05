library(tidyverse)
library(readxl)
camml_voc = read_csv("parsed_data/camml/camml_voc_data_gcid.csv")

camml_vocs = camml_voc |>
  mutate(concentration = replace_na(concentration, 0)) |>
  group_by(voc) |>
  summarize(avg_conc = mean(concentration, na.rm = TRUE)) |>
  arrange(desc(avg_conc))

write_csv(camml_vocs, "exploratory/measured_vocs.csv")

# Then read in our spreadsheet of correction factors
cf = read_xlsx("correction_factors/pid_correction_factors.xlsx")
ggplot(cf, aes(x = nC, y = `CF 10.6`, color = Type)) +
  geom_line() +
  scale_y_log10()
# Well, that's a mess

cf = cf |>
  mutate(Type2 = case_when(Type == "Alkane" & Subtype == "Straight" ~ "n-Alkane",
                           Type == "Alkane" ~ "Branched Alkane",
                           TRUE ~ Type))

cf |>
  filter(!is.na(`CF 10.6`)) |>
  ggplot(aes(x = nC, y= `CF 10.6`, color = Type2)) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(2, 12, by = 2))

cf |>
   filter(is.na(`CF 10.6`)) |>
  count(Type2, nC)

# Alright, I think for this project, interpolating to the nearest Type2 with the same nC (or the nearest nc)
# if going to be good enough
avg_cf = cf |>
  group_by(Type2, nC) |>
  summarize(avg_cf = mean(`CF 10.6`, na.rm = TRUE), .groups = "drop")

interpolated_cf = left_join(cf, avg_cf, by = c("Type2", "nC")) |>
  mutate(cf = coalesce(`CF 10.6`, avg_cf)) |>
  mutate(cf = case_when(!is.na(cf) ~ cf,
                     Type == "Aromatic" ~ mean(cf[Type == "Aromatic"], na.rm = TRUE),
                     Type == "Alkane" & nC == 12 ~ mean(cf[Type2 == "n-Alkane" & nC == 11], na.rm = TRUE),
                     voc == "a-pinene" ~ cf[voc == "isoprene"]),
         is_interpolated = is.na(`CF 10.6`)) |>
  select(voc:Subtype, cf, is_interpolated)
interpolated_cf
write_csv(interpolated_cf, "correction_factors/interpolated_correction_factors.csv")
