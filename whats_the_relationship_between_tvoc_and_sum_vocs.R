library(tidyverse)

data_for_modeling_raw = read_csv("parsed_data/camml/camml_combined_data.csv") #Aligned CAMML and SPOD data
camml_voc_data_gcid = read_csv("parsed_data/camml/camml_voc_data_gcid.csv") # Other VOCs
camml_voc_data_gcid

voc_avg_conc = camml_voc_data_gcid |>
  group_by(voc, units) |>
  summarize(avg_concentration = mean(concentration, na.rm = TRUE),
            .groups = "drop") |>
  mutate(avg_concentration = ifelse(avg_concentration < 0, 0.001, avg_concentration)) |>
  # replace_na(list(avg_concentration = 0.01)) |>
  arrange(avg_concentration) |>
  ungroup() |>
  mutate(voc_f = factor(voc, levels = voc),
         voc_f = fct_lump_min(voc_f, min = 0.1, w = avg_concentration)) |>
  group_by(voc_f) |>
  summarize(avg_concentration = sum(avg_concentration), .groups = "drop")

ggplot(voc_avg_conc, aes(x = voc_f, y = avg_concentration)) +
  geom_point() +
  coord_flip() +
  scale_y_log10()

# THat's I guess interesting? Light alkanes are the most common, followed by heavier alkanes.

# Next interesting question: Which of these explain the variation in the sum? I think that's just
# a modified version of R2

sos = function(x) {
  xbar = mean(x, na.rm = TRUE)
  sum( (x - xbar)^2, na.rm = TRUE)
}

sos_metric = function(voc_conc, sum_voc_conc) {
  m = is.finite(voc_conc)
  1 - sos(sum_voc_conc[m] - voc_conc[m]) / sos(sum_voc_conc[m])
}

camml_sum_vocs = camml_voc_data_gcid |>
  select(start_time_lst, voc, units, concentration, gc_id) |>
  mutate(concentration = replace_na(concentration, 0)) |>
  group_by(gc_id) |>
  mutate(sum_vocs = sum(concentration, na.rm = TRUE)) |>
  ungroup()


camml_sum_vocs |>
  filter(voc == "styrene") |>
  summarize(sos_tot = sos(sum_vocs),
            sos_ms = sos(sum_vocs - concentration),
            sos_metric = sos_metric(concentration, sum_vocs))


camml_fve = camml_sum_vocs |>
  mutate(voc = ifelse(voc %in% levels(voc_avg_conc$voc_f), voc, "Other")) |>
  group_by(voc) |>
  summarize(frac_variance_explained = sos_metric(concentration, sum_vocs)) |>
  mutate(voc_f = factor(voc, levels = levels(voc_avg_conc$voc_f)))
ggplot(camml_fve, aes(x = voc_f, y = frac_variance_explained)) +
  geom_point() +
  scale_y_log10() +
  coord_flip()
# That's kinda interesting - the highest concentrations aren't always the highest fraction of
# variance explained

wide_df = inner_join(camml_fve, voc_avg_conc, by = "voc_f")
plot_df = wide_df |>
  pivot_longer(c(frac_variance_explained, avg_concentration), names_to = "metric")
ggplot(plot_df, aes(x = voc_f, y = value)) +
  geom_point() +
  scale_y_log10() +
  coord_flip() +
  facet_wrap("metric", nrow =1, scales = "free_y")

ggplot(wide_df, aes(x = avg_concentration, y = frac_variance_explained)) +
  geom_point()

# Seems reasonble, but therer's a lot of spread here.

# Now, how does it compare to the SPOD?
df1 = data_for_modeling_raw |>
  select(gc_id, tvoc, benzene)
df2 = camml_sum_vocs |>
  select(start_time_lst, gc_id, sum_vocs) |>
  group_by(gc_id) |>
  slice_head(n= 1)

comparison_df = inner_join(df1, df2, by = "gc_id")
ggplot(comparison_df, aes(x = tvoc, y = sum_vocs)) +
  geom_point()

ggplot(comparison_df, aes(x = tvoc, y = benzene)) +
  geom_point()
# These graphs look odd to me...

# Let's compare with the data from 3rd pass
X = readRDS("xgboost_numeric_model_info.RDS")
X$data_spod_training |>
  ggplot(aes(x = tvoc, y = log(benzene + 0.1))) +
  geom_point()
# No, those are the same, with fewer points.

# Those are also some crazy high readings for tvoc! What is going on there?
X$data_spod_training |>
  ggplot(aes(x = factor(deployment_id), y = tvoc)) +
  geom_boxplot()
# Looks like the SPOD is reported in ppb, which leads to some interesting questions the other way...

X$data_spod_training |>
  group_by(factor(deployment_id)) |>
  summarize(mean = mean(tvoc, na.rm = TRUE),
            median = median(tvoc, na.rm = TRUE),
            q05 = quantile(tvoc, 0.05, na.rm = TRUE),
            q95 = quantile(tvoc, 0.95, na.rm = TRUE),
            n = sum(is.finite(tvoc)))
# ALright, I'm not seeing any evidence for a units issue.
# But I do really wonder about baseline and t/p/rh corrections?

camml_other_raw = read_csv("parsed_data/camml/camml_other_data_gcid.csv")

camml_other = camml_other_raw |>
  mutate(value = case_when(measurement == "O3" & units == "ppmV" ~ value * 1000,
                           measurement == "VOC(pid)" & units == "IBeq_ppbV" ~ value,
                           TRUE ~ value),
         units = case_when(measurement == "O3" & units == "ppmV" ~ "ppbV",
                           measurement == "VOC(pid)" & units == "IBeq_ppbV" ~ "ppbV",
                           TRUE ~ units))

# Check that our units are correct
camml_other |>
  count(measurement, units)

camml_other_wide = camml_other |>
  select(gc_id, gc_deployment_id, start_time_lst, end_time_lst, measurement, units, value) |>
  pivot_wider(id_cols = c(gc_id, gc_deployment_id, start_time_lst, end_time_lst),
              names_from = measurement,
              values_from = value)
camml_other_wide_gc = camml_other_wide |>
  group_by(gc_id,gc_deployment_id) |>
  summarize(across(WindSpeed:CO2, mean, na.rm = TRUE),
            start_time_lst = min(start_time_lst))

ggplot(camml_other_wide_gc, aes(x = `T(ambient)`, y = `VOC(pid)`)) +
  geom_point() +
  scale_y_continuous(limits = c(-100, 100))

ggplot(camml_other_wide_gc, aes(x = `P(barometric)`, y = `VOC(pid)`)) +
  geom_point() +
  scale_y_continuous(limits = c(-100, 100))

ggplot(camml_other_wide_gc, aes(x = RH, y = `VOC(pid)`)) +
  geom_point() +
  scale_y_continuous(limits = c(-100, 100))

ggplot(camml_other_wide_gc, aes(x = `T(shelter)`, y = `VOC(pid)`)) +
  geom_point() +
  scale_y_continuous(limits = c(-100, 100))

# No obvious crazy trends. I think we'll actually need to do a baseline adjustment first and
# then look again.

# Alright, how do I want to do the baseline adjustment?
# Probably using the 1-min data, look at the past hour? day? week? and take the minimum? 5th percentile?
# median? Well, let's look at all of them and see how they vary?
# I don't think hour makes sense, we should do maybe 1 day, 3 days, and 7 days?
# And look at min, 5th, and median - that's 9 combinations
library(slider)
library(lubridate)
# camml_other_wide |>
#   ungroup() |>
#   rename(tvoc = `VOC(pid)`) |>
#   mutate(tvoc_min_1day = slide_index_dbl(tvoc, start_time_lst, min, .before = days(1), .complete = FALSE, na.rm = TRUE))
# The above code is too slow. We need to do better, which we can using cumsums
# Actually that only works fi we're calculating means. But padding still speeds the calculations up a lot.
# Let's be more intelligent about the pad:
camml_other_wide = camml_other_wide |>
  mutate(start_time_lst_minute = floor_date(start_time_lst, "minute"))
deployment_timing_df = camml_other_wide |>
  filter(!is.na(gc_deployment_id)) |>
  group_by(gc_deployment_id) |>
  summarize(first = min(start_time_lst_minute),
            last = max(start_time_lst_minute))
pad_df = deployment_timing_df |>
  group_by(gc_deployment_id) |>
  summarize(data.frame(start_time_lst_minute = seq(first, last, by = "mins"))) |>
  ungroup() |>
  rename(pad_deployment_id = gc_deployment_id)
# pad_df = data.frame(start_time_lst_minute = seq(min(camml_other_wide$start_time_lst_minute),
#                                                 max(camml_other_wide$start_time_lst_minute),
#                                                 by = "mins")) |>
#   as_tibble()
pad_df
camml_other_wide_pad = full_join(camml_other_wide, pad_df)

# The following is a lot faster if we suppress warnings
swmin = function(..., na.rm = TRUE) {
  suppressWarnings(min(..., na.rm = na.rm))
}

camml_other_wide_slide = camml_other_wide_pad |>
  rename(tvoc = `VOC(pid)`) |>
  filter(!is.na(pad_deployment_id)) |>
  group_by(pad_deployment_id) |>
  mutate(tvoc_min_1day = slide_dbl(tvoc, swmin, .before = 12*60*1, .complete = FALSE, na.rm = TRUE,
                                   .after = 12*60*1),
         tvoc_min_3day = slide_dbl(tvoc, swmin, .before = 12*60*3, .complete = FALSE, na.rm = TRUE,
                                   .after = 12*60*3),
         tvoc_min_7day = slide_dbl(tvoc, swmin, .before = 12*60*7, .complete = FALSE, na.rm = TRUE,
                                   .after = 12*60*7),
         tvoc_q05_1day = slide_dbl(tvoc, quantile, .before = 12*60*1, .complete = FALSE, na.rm = TRUE,
                                   .after = 12*60*1, probs = 0.05),
         tvoc_q05_3day = slide_dbl(tvoc, quantile, .before = 12*60*3, .complete = FALSE, na.rm = TRUE,
                                   .after = 12*60*3, probs = 0.05),
         tvoc_q05_7day = slide_dbl(tvoc, quantile, .before = 12*60*7, .complete = FALSE, na.rm = TRUE,
                                   .after = 12*60*7, probs = 0.05),
         tvoc_med_1day = slide_dbl(tvoc, median, .before = 12*60*1, .complete = FALSE, na.rm = TRUE,
                                   .after = 12*60*1),
         tvoc_med_3day = slide_dbl(tvoc, median, .before = 12*60*3, .complete = FALSE, na.rm = TRUE,
                                   .after = 12*60*3),
         tvoc_med_7day = slide_dbl(tvoc, median, .before = 12*60*7, .complete = FALSE, na.rm = TRUE,
                                   .after = 12*60*7))
# # This is ... very slow ....
# #

# camml_other_wide_slide = readRDS("camml_other_wide_slide.RDS")

ggplot(camml_other_wide_slide, aes(x = start_time_lst, y = tvoc_q05_7day)) +
  geom_line()

camml_other_wide_slide |>
  filter(!is.na(pad_deployment_id),
         !is.na(tvoc_q05_7day)) |>
  pivot_longer(tvoc_min_1day:tvoc_med_7day) |>
  ggplot(aes(x = start_time_lst, y = value)) +
  geom_line(aes(color = name)) +
  geom_point(aes(y = tvoc), size = 0.8) +
  facet_wrap("gc_deployment_id", scales = "free")
# Weirdly, none of these really look like what I'd expect?

camml_other_wide_slide |>
  filter(!is.na(pad_deployment_id),
         !is.na(tvoc_q05_7day),
         pad_deployment_id == 10) |>
  pivot_longer(tvoc_min_1day:tvoc_med_7day, names_prefix = "tvoc_", names_sep = "_",
               names_to = c("metric", "averaging_time")) |>
  mutate(value = ifelse(metric == "min" & value == -Inf, NA, value)) |>
  ggplot(aes(x = start_time_lst, y = value)) +
  geom_line(aes(color = metric)) +
  geom_point(aes(y = tvoc), size = 0.8) +
  facet_grid(cols = vars(metric), rows = vars(averaging_time))

# Ok, that's useful. I think I'm finding that q05 is definitely the right metric to use, but
# I'm not sure about the averaging time.

camml_other_wide_slide |>
  filter(!is.na(pad_deployment_id),
         !is.na(tvoc_q05_7day)) |>
  pivot_longer(tvoc_min_1day:tvoc_med_7day, names_prefix = "tvoc_", names_sep = "_",
               names_to = c("metric", "averaging_time")) |>
  mutate(value = ifelse(metric == "min" & value == -Inf, NA, value)) |>
  filter(metric == "q05") |>
  ggplot(aes(x = start_time_lst, y = value)) +
  geom_line(aes(color = averaging_time)) +
  geom_point(aes(y = tvoc), size = 0.8) +
  facet_wrap("gc_deployment_id", scales = "free")

# Something is weird about deployment 15
# Hrm. There's stilll something odd about deployment 15
camml_other_wide_slide |>
  filter(gc_deployment_id == 15) |>
  tail(n = 25) |>
  print(width = Inf)
