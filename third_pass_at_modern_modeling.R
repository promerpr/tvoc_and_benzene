library(tidyverse)
library(rsample)
library(lubridate)
library(recipes)
library(parsnip)
library(workflows)
library(naniar)
library(tune)
library(sf)
library(slider)
library(doParallel)
library(vip)


# First, load all the datasets that we'll want:
data_for_modeling_raw = read_csv("parsed_data/camml/camml_combined_data.csv") #Aligned CAMML and SPOD data
atc_conc_2017_raw = read_csv("../AirToxScreen/parsed_data_2017/co_concentration_by_sector.csv") #AirToxScreen 2017
wide_camml_tvoc_wd_raw = read_csv("parsed_data/camml/camml_wide_spod_wind.csv") #Wind Direction and TVOC in 5-min intervals
co_ct = st_read("X:/Shared drives/_CDPHE TEEO Data/Geospatial Boundaries/colorado_census_tracts/colorado_census_tracts.shp") # Census tracts in colorado

# Second, clean up all the datasets that we'll need - I'm planning to include basically all the
# features we might want, and then remove the ones we don't want later.
atc_benzene = atc_conc_2017_raw |>
  filter(pollutant_name == "BENZENE") |>
  filter(sector %in% c("total", "np_oil_gas"))  |>
  select(tract, sector, ambient_conc) |>
  pivot_wider(names_from = sector, names_prefix = "atc_benzene_", values_from = ambient_conc)

convert_to_weather_wind_direction = function(math_wind_direction) {
  # convert to degrees and re-orient
  weather_wind_direction = 270 - math_wind_direction * 360 / (2 * pi)
  weather_wind_direction %% 360
}

wide_camml_tvoc_wd = wide_camml_tvoc_wd_raw |>
  mutate(across(starts_with("WindDirection_"), convert_to_weather_wind_direction)) |>
  rowwise() |>
  mutate(wind_sd = sd(c_across(starts_with("WindDirection_")), na.rm = TRUE),
         tvoc_sd = sd(c_across(starts_with("tvoc_")), na.rm = TRUE)) |>
  ungroup()


# Treat flags and operational phase a factors with an explicit NA level.
# Deal with NA's in source_direction_degrees
# Convert to weather wind direction
# Fix units for TVOC in deployment 9
# TODO(): Check the units for TVOC in deployment 9 with Jason
data_for_modeling_int = data_for_modeling_raw |>
  mutate(flags = factor(flags),
         flags = fct_explicit_na(flags)) |>
  mutate(operational_phase = fct_explicit_na(operational_phase)) |>
  mutate(source_direction_degrees = as.numeric(source_direction_degrees)) |>
  replace_na(list(source_direction_degrees = 0)) |>
  mutate(WindDirection = convert_to_weather_wind_direction(WindDirection)) |>
  mutate(tvoc = ifelse(deployment_id == 9, tvoc * 1000, tvoc))

# Add tractfips information (so we can add AirToxScreen info)
data_for_modeling = data_for_modeling_int |>
  st_as_sf(coords = c("camml_lon", "camml_lat"), crs = "+proj=longlat", remove = FALSE) |>
  st_join(co_ct) |>
  st_drop_geometry() |>
  select(-countyfips, -statefips)

# Add day- and week-long averages of the meteorological variables and TVOCs:
data_for_modeling = data_for_modeling |>
  group_by(deployment_id) |>
  mutate(across(c(tvoc, RH, `T(ambient)`, `P(barometric)`),
                .fns = list(day = \(x) slide_index_dbl(x, start_time_lst, mean, .before = days(1), .complete = FALSE),
                            week = \(x) slide_index_dbl(x, start_time_lst, mean, .before = days(7), .complete = FALSE)
                ))) |>
  ungroup()

# Then combine all the data together
combined_data_for_modeling =
  left_join(data_for_modeling, atc_benzene, by = c("tractfips" = "tract")) |>
  left_join(wide_camml_tvoc_wd, by = "gc_id")


# Split into training and testing by deployments:
deployments = data_for_modeling |>
  na.omit() |>
  distinct(deployment_id, deployment_name)

set.seed(1435235)
deployment_split = initial_split(deployments, prop = 0.8)
deployment_training = training(deployment_split)
deployment_testing = testing(deployment_split)

data_training = semi_join(combined_data_for_modeling, deployment_training)
data_testing = semi_join(combined_data_for_modeling, deployment_testing)

### Start with some exploratory data analysis on benzene, tvoc, and wind speed: ----

ggplot(data_training, aes(x = start_time_lst, y = benzene)) +
  geom_point() +
  scale_y_log10()
# No obvious trends over time

ggplot(data_training, aes(x = factor(deployment_id), y = benzene)) +
  geom_violin() +
  geom_jitter(width = 0.25, size = 0.1, alpha = 0.5, color = "gray40") +
  scale_y_log10()
# There are some differences between deployments, but I don't see an overall trend.

ggplot(data_training, aes(x = factor(hour(start_time_lst)), y = benzene)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 1))
# Benzene measurements are typically lower than I'd expect - almost all of them are lower than 1 ppb
# We maybe see a little diurnal cycle, with higher concentrations of Benzene around noon and lower at night
# Maybe because they're from mobile sources?

ggplot(data_training, aes(x = benzene)) +
  geom_histogram(fill = "lightblue", color = "blue") +
  scale_x_log10()
# Benzene follows an approximate log-normal distribution.
# Typical concentrations are around 0.1 ppb.
sum(data_training$benzene > 1, na.rm = TRUE) / nrow(data_training)
# Just over 3% of the data points are over 1 ppb - that could be an option if we wanted to
# take a classification approach instead, although that's still quite unbalanced.


# Now let's look at tvoc measurements
ggplot(data_training, aes(x = start_time_lst, y = tvoc)) +
  geom_point() +
  scale_y_log10()

ggplot(data_training, aes(x = factor(deployment_id), y = tvoc)) +
  geom_violin() +
  geom_jitter(width = 0.25, size = 0.1, alpha = 0.5, color = "gray40") +
  scale_y_log10()

# Checking to see what this looks like in all deployments:
combined_data_for_modeling |>
  ggplot(aes(x = factor(deployment_id), y = tvoc)) +
  geom_violin() +
  geom_jitter(width = 0.25, size = 0.1, alpha = 0.5, color = "gray40") +
  scale_y_log10()

data_training |>
  mutate(log_tvoc = ifelse(tvoc > 0, log10(tvoc), -2)) |>
  ggplot(aes(x = log_tvoc)) +
  geom_histogram(color = "red", fill = "pink")
# I'm not sure what to do with the tvoc - should I log transform it?

ggplot(data_training, aes(x = factor(deployment_id), y = tvoc)) +
  geom_violin() +
  geom_jitter(width = 0.25, size = 0.1, alpha = 0.5, color = "gray40")
# There are a surprising number of points that are significantly below zero.
# I think that makes trying to log transform TVOC a bad idea.

ggplot(data_training, aes(x = tvoc)) +
  geom_histogram()
# That's actually not far off from normal. That's more evidence that log-transforming isn't necessary.

ggplot(data_training, aes(x = factor(hour(start_time_lst)), y = tvoc)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(-50, 50))
# There's basically no diurnal cycle in TVOC measurements, which is interesting.

# Do a quick check on wind speed
ggplot(data_training, aes(x = WindSpeed)) +
  geom_histogram(fill = "seagreen", color = "darkgreen")
# That's roughly the shape I'd expect for a squared quantity.

### Down-selecting to variables that would be collected by an SPOD ----
data_spod_training = data_training |>
  select(-(SolarRadiation:v)) |>
  na.omit()

### Creating cross-folds ----
folds_spod_training = group_vfold_cv(data_spod_training, group = deployment_id)


### Building some very traditional models by hand to see how well we can do there:
ggplot(data_spod_training, aes(x = sqrt(WindSpeed), y = log10(benzene), color = tvoc)) +
  geom_point()

# First, using lm:
lm_formula = log10(benzene) ~ WindSpeed * tvoc + WindDirection + `T(ambient)` + tvoc_low + tvoc_high +
  atc_benzene_total + wind_sd
lm(lm_formula,
   data = data_spod_training) |>
  arm::display(digits = 4)
# That's an R2 of 0.29, which isn't bad, but we're also not doing cross-validation.

# Second, using parsnip
lm_mod = parsnip::linear_reg() |>
  set_engine("lm")
fit_resamples(lm_mod, lm_formula, resamples = folds_spod_training) |>
  collect_metrics()
# This is giving me a bunch of warning, but I'm going to ignore them for now.
# Using this linear model, I'm now getting an R2 of 0.14.
# That's actually a bit better than I was expecting.

# What happens if I use my full recipe with all the predictors, but fit it to a linear model?
# Let's specify that in a recipe:
rec_spod_v1 = recipe(benzene ~ ., data = data_spod_training) |>
  update_role(end_time_lst, gc_id, gc_end_time, new_role = "ID") |>
  update_role(deployment_id, deployment_name, pad_name, pad_lat, pad_lon,
              camml_lat, camml_lon, tractfips, new_role = "ID") |>
  step_log(benzene, offset = 0.1) |>
  step_date(start_time_lst, features = c("dow", "month")) |>
  step_time(start_time_lst, features = "hour", keep_original_cols = FALSE) |>
  step_mutate(start_time_lst_hour = factor(start_time_lst_hour, levels = unique(start_time_lst_hour)),
              wind_offset = abs(WindDirection - source_direction_degrees)) |>
  step_rm(starts_with("WindDirection_"), starts_with("tvoc_"), -tvoc_day, -tvoc_week, -tvoc_sd) |>
  step_rm(flags) |> # This is more of an outcome than a predictor
  step_nzv(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_rm(start_time_lst_month, operational_phase)

prepped_data = prep(rec_spod_v1, data = data_spod_training) |>
  bake(new_data = NULL, all_predictors(), all_outcomes())
print(prepped_data, width = Inf)
ncol(prepped_data)

# OK, I think we're in a position to run the linear model with this recipe:
fit_resamples(lm_mod, rec_spod_v1, resamples = folds_spod_training) |>
  collect_metrics()
# The metrics don't actually look that bad - rmse of 1.75, rsq  of 0.276

### Compare to a random forest model ----
rf_mod = parsnip::rand_forest(trees = 1000, mtry = 8) |>
  set_mode("regression") |>
  set_engine("ranger", importance = "permutation")
fit_resamples(rf_mod, rec_spod_v1, resamples = folds_spod_training) |>
  collect_metrics()
# The random forest does a lot better though: RMSE of 0.48, r2 = 0.38

### Tune the random forest model:
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

rf_tune = parsnip::rand_forest(trees = 1000, mtry = tune(), min_n = tune()) |>
  set_mode("regression") |>
  set_engine("ranger", importance = "permutation")



set.seed(1542385)
ranger_tune = tune_grid(rf_tune, rec_spod_v1, resamples = folds_spod_training, grid = 15)

stopCluster(cl)

autoplot(ranger_tune)
collect_metrics(ranger_tune) |>
  filter(.metric == "rmse") |>
  arrange(desc(mean))

# The results seem pretty clean that mtry should be 5.
# There's no really trend in the minimal node size, so I'm going to leave it as the default
rf_mod2 = parsnip::rand_forest(trees = 1000, mtry = 5) |>
  set_mode("regression") |>
  set_engine("ranger", importance = "permutation")
fit_resamples(rf_mod2, rec_spod_v1, resamples = folds_spod_training) |>
  collect_metrics()
# rmse of 0.48, r2 = 0.38 (no improvement from the un-tuned model)

baked_data_for_vip = prep(rec_spod_v1, data = data_spod_training) |>
  bake(new_data = NULL, all_predictors(), all_outcomes())
fit_for_vip = fit(rf_mod2, benzene ~ ., data = baked_data_for_vip)
vip(fit_for_vip, geom = "point")

# That's really interesting that the tvoc_sd is such an important parameter. I wonder why?
# Also the past week of barometric pressure?
# I wonder if the week parameters are somehow getting at deployment-specific information?

ggplot(data_spod_training, aes(x = tvoc_sd, y = benzene)) +
  geom_point() +
  scale_x_log10()

ggplot(data_spod_training, aes(x = tvoc, y = benzene)) +
  geom_point() +
  scale_x_log10()
# That's really interesting - there does seem to be a clear relationship between tvoc_sd and benzene
# Maybe more on the ceiling of the relationship.

### Can we use xgbooost? ----
xgb_mod = parsnip::boost_tree(
  trees = tune(),
  mtry = tune(),
  min_n = tune(),
  learn_rate = 0.01, # Taken from https://juliasilge.com/blog/board-games/
) |>
  set_engine("xgboost") |>
  set_mode("regression")
