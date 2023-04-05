# Try to correct tvoc measurements for baseline drift

library(tidyverse)
library(lubridate)
library(slider)
camml_other_raw = read_csv("parsed_data/camml/camml_other_data_wide_gcid.csv")

camml_other_wide = camml_other_raw |>
  rename(tvoc = voc_pid,
         tvoc_units = voc_pid_units)


# The following is a lot faster if we suppress warnings
swmin = function(..., na.rm = TRUE) {
  suppressWarnings(min(..., na.rm = na.rm))
}

camml_other_wide_slide = camml_other_wide |>
  group_by(deployment_id) |>
  mutate(tvoc_min_1day = slide_index_dbl(tvoc, start_time_lst, swmin, .before = 1*hours(24)/2,
                                         .complete = FALSE, na.rm = TRUE,
                                         .after = 1*hours(24)/2),
         tvoc_min_3day = slide_index_dbl(tvoc, start_time_lst, swmin, .before = 3*hours(24)/2,
                                         .complete = FALSE, na.rm = TRUE,
                                         .after = 3*hours(24)/2),
         tvoc_min_7day = slide_index_dbl(tvoc, start_time_lst, swmin, .before = 7*hours(24)/2,
                                         .complete = FALSE, na.rm = TRUE,
                                         .after = 7*hours(24)/2),
         tvoc_q05_1day = slide_index_dbl(tvoc, start_time_lst, quantile, .before = 1*hours(24)/2,
                                         .complete = FALSE, na.rm = TRUE,
                                         .after = 1*hours(24)/2, probs = 0.05),
         tvoc_q05_3day = slide_index_dbl(tvoc, start_time_lst, quantile, .before = 3*hours(24)/2,
                                         .complete = FALSE, na.rm = TRUE,
                                         .after = 3*hours(24)/2, probs = 0.05),
         tvoc_q05_7day = slide_index_dbl(tvoc, start_time_lst, quantile, .before = 7*hours(24)/2,
                                         .complete = FALSE, na.rm = TRUE,
                                         .after = 7*hours(24)/2, probs = 0.05),
         tvoc_med_1day = slide_index_dbl(tvoc, start_time_lst, median, .before = 1*hours(24)/2,
                                         .complete = FALSE, na.rm = TRUE,
                                         .after = 1*hours(24)/2),
         tvoc_med_3day = slide_index_dbl(tvoc, start_time_lst, median, .before = 3*hours(24)/2,
                                         .complete = FALSE, na.rm = TRUE,
                                         .after = 3*hours(24)/2),
         tvoc_med_7day = slide_index_dbl(tvoc, start_time_lst, median, .before = 7*hours(24)/2,
                                         .complete = FALSE, na.rm = TRUE,
                                         .after = 7*hours(24)/2))

camml_other_wide_slide |>
  filter(!is.na(tvoc_q05_7day)) |>
  pivot_longer(tvoc_min_1day:tvoc_med_7day) |>
  ggplot(aes(x = start_time_lst, y = value)) +
  geom_line(aes(color = name)) +
  geom_point(aes(y = tvoc), size = 0.8) +
  facet_wrap("deployment_id", scales = "free")
# Hrm. Some oddities there. Not sure what's going on.

camml_slide_long = camml_other_wide_slide |>
  filter(!is.na(tvoc_q05_7day)) |>
  select(deployment_id, start_time_lst, tvoc, tvoc_min_1day:tvoc_med_7day, gc_id) |>
  pivot_longer(tvoc_min_1day:tvoc_med_7day, names_prefix = "tvoc_", names_sep = "_",
               names_to = c("metric", "averaging_time")) |>
  mutate(value = ifelse(metric == "min" & (!is.finite(value)), NA, value)) |>
  ungroup()

camml_slide_long |>
  filter(deployment_id == 15) |>
  ggplot(aes(x = start_time_lst, y = value)) +
  geom_line(aes(color = metric)) +
  geom_point(aes(y = tvoc), size = 0.8) +
  facet_grid(cols = vars(metric), rows = vars(averaging_time))

# Which metric do we want? I wonder if this is an empirical question - we have 9 potential baseline corrections,
# and we want to test which one of them works best.
# And the best way to do that is going to be to plot them against what we think we should be measuring -
# sum VOCs as measured by the GC

camml_voc = read_csv("parsed_data/camml/camml_voc_data_gcid.csv")
voc_cf = read_csv("correction_factors/interpolated_correction_factors.csv")
camml_voc = camml_voc |>
  # select(start_time_lst, voc, units, concentration, gc_id, deployment_id) |>
  mutate(concentration = replace_na(concentration, 0)) |>
  group_by(gc_id) |>
  mutate(sum_vocs = sum(concentration, na.rm = TRUE)) |>
  ungroup()
camml_voc
camml_sum_voc = camml_voc |>
  left_join(voc_cf, by = "voc") |>
  group_by(gc_id) |>
  summarize(sum_vocs = sum(concentration, na.rm = TRUE),
            sum_vocs_corrected = sum(concentration * cf))

ggplot(camml_sum_voc, aes(x = sum_vocs, y = sum_vocs_corrected)) +
  geom_point() +
  geom_abline(color = "red")
camml_sum_voc
camml_avg_tvoc = camml_slide_long |>
  mutate(baseline_corrected_tvoc = tvoc - value) |>
  group_by(gc_id, metric, averaging_time) |>
  summarize(avg_tvoc = mean(tvoc, na.rm = TRUE),
            avg_tvoc_blc = mean(baseline_corrected_tvoc),
            .groups = "drop") |>
  mutate(blc_option = as.numeric(interaction(averaging_time, metric)))
camml_avg_tvoc

camml_comp_df = inner_join(camml_sum_voc, camml_avg_tvoc, by = "gc_id")

camml_comp_df |>
  filter(avg_tvoc_blc >= 1e-4) |>
ggplot(aes(x = sum_vocs, y = avg_tvoc_blc)) +
  geom_hex() +
  geom_abline(color = "red") +
  facet_wrap("blc_option") +
  scale_x_log10() +
  scale_y_log10()

camml_comp_df |>
  filter(avg_tvoc_blc >= 1e-4) |>
ggplot(aes(x = sum_vocs_corrected, y = avg_tvoc)) +
  geom_hex() +
  geom_abline(color = "red") +
  facet_wrap("blc_option") +
  scale_x_log10() +
  scale_y_log10()


camml_comp_df |>
  group_by(blc_option) |>
  summarize(fit = list(lm(sum_vocs_corrected ~ avg_tvoc_blc))) |>
  rowwise(blc_option) |>
  summarize(filter(broom::tidy(fit), term == "avg_tvoc_blc"),
            broom::glance(fit)) |>
  print(width = Inf)
# I worry that this is highly driven by outliers ... maybe I can manually filter for outliers?
# Remove the 10 or 20% of points that deviate the most from 1:1 and then re-fit?

camml_comp_df = camml_comp_df |>
  group_by(blc_option) |>
  mutate(abs_diff = abs(sum_vocs - avg_tvoc_blc))

camml_comp_df |>
  filter(abs_diff <= quantile(abs_diff, 0.95, na.rm = TRUE)) |>
  summarize(fit = list(lm(sum_vocs ~ avg_tvoc_blc))) |>
  rowwise(blc_option) |>
  summarize(filter(broom::tidy(fit), term == "avg_tvoc_blc"),
            broom::glance(fit))



camml_comp_df |>
  filter(abs_diff <= quantile(abs_diff, 1, na.rm = TRUE)) |>
  ggplot(aes(x = sum_vocs, y = avg_tvoc_blc)) +
  geom_hex() +
  geom_abline(color = "red") +
  geom_smooth(method = "lm") +
  facet_wrap("blc_option")

camml_voc |>
  filter(sum_vocs >= 7500) |>
  arrange(desc(concentration)) |>
  select(voc, concentration, start_time_lst, gc_id) |>
  print(n = 20)

camml_voc |>
  filter(gc_id == 10789) |>
  arrange(desc(concentration)) |>
  select(voc, concentration, start_time_lst, gc_id) |>
  print(n = 20)
# That's pretty interesting. When we see the giant spikes in sum VOCs, it's predominately
# light alkanes (propane, ethane, n-butane)

#What about the ones where both tvoc and sum VOCs seem to be elevated?
elev_df = camml_comp_df |>
  filter(blc_option == 1, avg_tvoc_blc > 200, sum_vocs > 200) |>
  select(-averaging_time, -metric)

camml_voc |>
  semi_join(elev_df, by = "gc_id") |>
  arrange(desc(concentration)) |>
  select(voc, concentration, start_time_lst, gc_id) |>
  print(n = 20)

camml_voc |>
  filter(gc_id == 4018) |>
  arrange(desc(concentration)) |>
  select(voc, concentration, start_time_lst, gc_id) |>
  print(n = 20)

# That's also interesting, it that the composition of these plumes don't seem to be appreciably
# different from the composition of the giant plumes that the tvoc just doesn't register.
# So, what is different between those plumes?

camml_other_wide |>
  filter(gc_id %in% c(10789, 4018)) |>
  group_by(gc_id) |>
  summarize(across(wind_speed:co2, mean, na.rm = TRUE)) |>
  pivot_longer(-gc_id) |>
  pivot_wider(names_from = gc_id, values_from = value)

# Looking at the results, the only difference I can really see is in the temperature - 31 degC v 3 degC
# 3C is cold but not *that* cold

# The medians do suprisingly well - that makes me feel like I'm doing something wrong here? But
# maybe a constant offset isn't an issue.
# And shorter time periods seem to out-perform longer time periods.

camml_other_long_slide = camml_other_wide_slide |>
  filter(!is.na(tvoc_q05_7day)) |>
  # select(deployment_id, start_time_lst, tvoc, tvoc_min_1day:tvoc_med_7day, gc_id) |>
  pivot_longer(tvoc_min_1day:tvoc_med_7day, names_prefix = "tvoc_", names_sep = "_",
               names_to = c("metric", "averaging_time"), values_to = "tvoc_baseline") |>
  mutate(tvoc_baseline = ifelse(metric == "min" & (!is.finite(tvoc_baseline)), NA, tvoc_baseline)) |>
  ungroup()

camml_other_long_slide_sample = camml_other_long_slide |>
  slice_sample(n = 1e5)
ggplot(camml_other_long_slide_sample, aes(x = t_ambient, y = tvoc_baseline)) +
  geom_point() +
  geom_smooth() +
  facet_grid(cols = vars(metric), rows = vars(averaging_time)) +
  labs(title = "Effect of temperature on baseline")

ggplot(camml_other_long_slide_sample, aes(x = rh, y = tvoc_baseline)) +
  geom_point() +
  geom_smooth() +
  facet_grid(cols = vars(metric), rows = vars(averaging_time)) +
  labs(title = "Effect of RH on baseline")

# I'm not seeing any effect of temperature or RH on baseline.
# I wonder if we first need to do like a deployment-only adjustment?

ggplot(camml_other_long_slide, aes(x = deployment_id, y = tvoc_baseline)) +
  geom_jitter() +
  facet_grid(cols = vars(metric), rows = vars(averaging_time)) +
  labs(title = "Effect of deployment_id on baseline")

# well maybe I can do it just with a regression?
# deployment_id, t, RH
baseline_fits = camml_other_long_slide_sample |>
  group_by(metric, averaging_time) |>
  summarize(fit = list(lm(tvoc_baseline ~ rh + t_ambient + factor(deployment_id))),
            .groups = "drop")
baseline_fits |>
  rowwise(metric, averaging_time) |>
  summarize(broom::tidy(fit)) |>
  filter(!startsWith(term, "factor(")) |>
  arrange(term) |>
  print(n = Inf)

# Hrm. What does this mean?
# Ok, let's not blindly chase r2 here - we're trying to come up with a plausible idea of how
# these SPOD's work and what they might actually be measuring.
# By that end, we shouldn't be surprised that the different metrics have different results, since
# they are actually measuring different things.



