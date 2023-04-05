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
data_spod_testing = data_testing |>
  select(-(SolarRadiation:v)) |>
  na.omit()
### Creating cross-folds ----
folds_spod_training = group_vfold_cv(data_spod_training, group = deployment_id)


### Building some very traditional models by hand to see how well we can do there: ----
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
cl <- makePSOCKcluster(6)
registerDoParallel(cl)

rf_tune = parsnip::rand_forest(trees = 1000, mtry = tune(), min_n = tune()) |>
  set_mode("regression") |>
  set_engine("ranger", importance = "permutation")



set.seed(1542385)
ranger_tune = tune_grid(rf_tune, rec_spod_v1, resamples = folds_spod_training, grid = 15)

# stopCluster(cl)

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
xgb_fit1 = parsnip::boost_tree(
  trees = 1500,
  mtry = 10,
  min_n = 10,
  learn_rate = 0.01
) |>
  set_engine("xgboost") |>
  set_mode("regression")

rec_spod_xgb = rec_spod_v1 |>
  step_dummy(all_nominal_predictors())
xgb_resamples = fit_resamples(xgb_fit1, rec_spod_xgb, resamples = folds_spod_training)
xgb_resamples |>
  collect_metrics()
# Bit of an improvement over the random forest: rmse of 0.44, r2 of 0.41


#### Can we tune?
xgb_tune = parsnip::boost_tree(
  trees = tune(),
  mtry = tune(),
  min_n = tune(),
  learn_rate = 0.01, # Taken from https://juliasilge.com/blog/board-games/
) |>
  set_engine("xgboost") |>
  set_mode("regression")


library(finetune)
set.seed(6789)
xgb_tune_res = tune_race_anova(xgb_tune, rec_spod_xgb, resamples = folds_spod_training,
                               grid = 40, control_race(verbose_elim = TRUE))
xgb_tune_res

# That was really quick! Let's try to take a look at the results
plot_race(xgb_tune_res)
show_best(xgb_tune_res, metric = "rmse")
autoplot(xgb_tune_res)
# What does it mean that the best results happened so early and then the results got worse?

# Ok, I don't quite get tune_race_anova. So let's skip the fancy tuning algorithm and just
# use tune().
xgb_tune_res2 = tune_grid(xgb_tune, rec_spod_xgb, resamples = folds_spod_training,
                               grid = 20)
autoplot(xgb_tune_res2)

# Let's try to control this more and do a full grid...but we don't want much more than 20 options
library(yardstick)
xgb_tune_res3 = tune_grid(xgb_tune, rec_spod_xgb, resamples = folds_spod_training,
                         grid = crossing(mtry = c(5, 10, 20),
                                         trees = seq(100, 1600, by = 150),
                                         min_n = c(5, 10, 20)),
                         metrics = yardstick::metric_set(rmse))
autoplot(xgb_tune_res3)

# Let's try expanding out to even more parameters that we could tune and leaving the full
# grid behind:
xgb_tune2 = parsnip::boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune(),  #https://juliasilge.com/blog/xgboost-tune-volleyball/
) |>
  set_engine("xgboost") |>
  set_mode("regression")

library(dials)
baked_data_following_xgb = prep(rec_spod_xgb, data = data_spod_training) |>
  bake(new_data = NULL, all_predictors())

param_set =
  xgb_tune2 |>
  extract_parameter_set_dials() |>
  finalize(x = baked_data_following_xgb)

xgb_parameter_combos =
  grid_latin_hypercube(
    param_set, #I'm not really sure what black magic is going on here and how we're automatically coming up with the parameter ranges...
      size = 70)


xgb_tune_res4 = tune_grid(xgb_tune2, rec_spod_xgb, resamples = folds_spod_training,
                          grid = xgb_parameter_combos)
# I don't really understand how long it takes to train various models - this seems like it
# should take the same amount of time as xgb_tune_res3, but it's taking a lot longer.

collect_metrics(xgb_tune_res4)
show_best(xgb_tune_res4, "rmse")
show_best(xgb_tune_res4, "rsq")
select_best(xgb_tune_res4, "rmse")
# I like the 2nd best option the best - it has was seems to be more reasonable numbers for
# min_n and learn_rate.

wanted_params = show_best(xgb_tune_res4, "rmse")[2, ]
wanted_params
xgb_tune3 = finalize_model(xgb_tune2,
               wanted_params)
# Now run on the full training dataset and collect predictions:
baked_data_following_xgb2 = prep(rec_spod_xgb, data = data_spod_training) |>
  bake(new_data = NULL, all_predictors(), all_outcomes())
xgb_fit2 = fit(xgb_tune3, benzene ~ ., data = baked_data_following_xgb2)
vip(xgb_fit2, geom = "point")

# Ok, that's quite interesting. We've gotten that the tvoc_sd and WindSpeed are the two
# best predictors.
# I'm curious about these week-long predictors. What exactly are they picking up on?

ggplot(baked_data_following_xgb2, aes(x = `P(barometric)_week`, y = benzene)) +
  geom_point()
ggplot(baked_data_following_xgb2, aes(x = `T(ambient)_week`, y = benzene)) +
  geom_point()
# There's definitely some structure, but it's really hard to see.

### SHAP values for xgboost? ----
# I don't really understand what SHAP is, but it seems to be a useful tool for
# understanding xgboost models.
library("SHAPforxgboost")

xgb_eng = extract_fit_engine(xgb_fit2)
baked_data_for_shap = prep(rec_spod_xgb, data = data_spod_training) |>
  bake(new_data = NULL,
       all_predictors(),
       composition = "matrix")
# have to do some manual adjustments to the column names
cn = colnames(baked_data_for_shap)
cn[grepl("[(]", cn)] = paste0("`", cn[grepl("[(]", cn)], "`")
colnames(baked_data_for_shap) = cn

shap_info =
  shap.prep(
    xgb_model = extract_fit_engine(xgb_fit2),
    X_train = baked_data_for_shap
  )
shap_info
shap.plot.summary(shap_info)
shap.plot.dependence(shap_info, x = "tvoc_sd")
shap.plot.dependence(shap_info, x = "WindSpeed")
shap.plot.dependence(shap_info, x = "WindSpeed",
                     smooth = FALSE, add_hist = TRUE)

shap.plot.dependence(shap_info, x = "WindSpeed", color_feature = "tvoc_sd",
                     smooth = FALSE, add_hist = TRUE)
shap.plot.dependence(shap_info, x = "tvoc_sd", color_feature = "WindSpeed",
                     smooth = FALSE, add_hist = TRUE)

# Hrm. I'm not sure what to make of these figures. I don't find the added histograms very useful.
# The summary plot I think it probably pretty useful, but it gives something that's pretty similar to
# a vip plot, just more complicated.
shap.plot.summary(shap_info, dilute = 10)

predict(xgb_fit2, new_data = baked_data_following_xgb2)
wanted_metrics <- metric_set(rmse, rsq, mae)
augment(xgb_fit2, new_data = baked_data_following_xgb2) |>
  wanted_metrics(truth = benzene, estimate = .pred)
# I guess that's the in-sample metrics, which are going to be way better than the out-of-sample
# metrics. But still, that's pretty fun to see.
plot_df = augment(xgb_fit2, new_data = baked_data_following_xgb2)
ggplot(plot_df, aes(x = benzene, y = .pred)) +
  geom_point() +
  geom_abline(color = "red") +
  geom_vline(xintercept = log(1)) +
  geom_vline(xintercept = log(10))

# Oh yes, this is on a log-log plot,
# But this plot is suggesting that this model might actually be useful.

# Ok, there are a couple more things that I want to work on, for the data-preprocessing.
# 1. Don't normalize the variables
# 2. Log transform tvoc_sd?
# 3. Make DoW and hr numeric variables

rec_spod_numeric = recipe(benzene ~ ., data = data_spod_training) |>
  step_rename(t_ambient = `T(ambient)`,
              p_barometric = `P(barometric)`,
              t_ambient_day = `T(ambient)_day`,
              p_barometric_day = `P(barometric)_day`) |>
  update_role(end_time_lst, gc_id, gc_end_time, new_role = "ID") |>
  update_role(deployment_id, deployment_name, pad_name, pad_lat, pad_lon,
              camml_lat, camml_lon, tractfips, new_role = "ID") |>
  step_log(benzene, offset = 0.1) |>
  step_date(start_time_lst, features = c("dow"), ordinal = TRUE) |>
  step_time(start_time_lst, features = "hour", keep_original_cols = FALSE) |>
  step_mutate(wind_offset = abs(WindDirection - source_direction_degrees)) |>
  step_rm(starts_with("WindDirection_"), starts_with("tvoc_"),
          -tvoc_day, -tvoc_week, -tvoc_sd, -tvoc_high, -tvoc_low) |>
  step_rm(flags) |> # This is more of an outcome than a predictor
  step_rm(ends_with("_week")) |>
  step_log(tvoc_sd, offset = 0.00001) |>
  step_ordinalscore(start_time_lst_dow) |>
  step_rm(operational_phase) |>
  step_nzv(all_nominal_predictors())

prep(rec_spod_numeric, data = data_spod_training) |>
  bake(new_data = NULL, all_predictors(), all_outcomes()) |>
  print(width = Inf)


baked_data_for_numeric_xgb = prep(rec_spod_numeric, data = data_spod_training) |>
  bake(new_data = NULL, all_predictors())

# Check for infinities and NA's and NaNs
baked_data_for_numeric_xgb |>
  rowwise() |>
  filter(if_any(.fns = \(x) !is.finite(x))) |>
  print(width = Inf)

# That seems reasonable. I think I like that set of features better.
# Now I guess I should repeat the tuning/fitting/examining process.
param_set_for_numeric_xgb =
  xgb_tune2 |>
  extract_parameter_set_dials() |>
  finalize(x = baked_data_for_numeric_xgb)

xgb_parameter_combos_numeric =
  grid_latin_hypercube(
    param_set, #I'm not really sure what black magic is going on here and how we're automatically coming up with the parameter ranges...
    size = 70)


xgb_tune_res_numeric = tune_grid(xgb_tune2, rec_spod_numeric,
                                 resamples = folds_spod_training,
                          grid = xgb_parameter_combos_numeric)


collect_metrics(xgb_tune_res_numeric)
show_best(xgb_tune_res4, "rmse")
show_best(xgb_tune_res_numeric, "rmse")
show_best(xgb_tune_res_numeric, "rsq")
show_best(xgb_tune_res4, "rsq")
# I like the 5thbest option the best - it has was seems to be more reasonable numbers for
# min_n and learn_rate.

wanted_params_numeric = show_best(xgb_tune_res_numeric, "rmse")[5, ]
xgb_numeric_finalized = finalize_model(xgb_tune2,
                                       wanted_params_numeric)
# Now run on the full training dataset and collect predictions:
baked_data_for_numeric_xgb_w_outcome = prep(rec_spod_numeric, data = data_spod_training) |>
  bake(new_data = NULL, all_predictors(), all_outcomes())
xgb_fit_numeric = fit(xgb_numeric_finalized, benzene ~ ., data = baked_data_for_numeric_xgb_w_outcome)
vip(xgb_fit_numeric, geom = "point")

xgb_eng = extract_fit_engine(xgb_fit_numeric)
baked_data_for_shap_numeric = prep(rec_spod_numeric, data = data_spod_training) |>
  bake(new_data = NULL,
       all_predictors(),
       composition = "matrix")
# # have to do some manual adjustments to the column names
# cn = colnames(baked_data_for_shap)
# cn[grepl("[(]", cn)] = paste0("`", cn[grepl("[(]", cn)], "`")
# colnames(baked_data_for_shap) = cn

shap_info_numeric =
  shap.prep(
    xgb_model = extract_fit_engine(xgb_fit_numeric),
    X_train = baked_data_for_shap_numeric
  )

shap.plot.summary(shap_info_numeric)
shap.plot.dependence(shap_info_numeric, x = "tvoc_sd", smooth = FALSE, add_hist = TRUE)
shap.plot.dependence(shap_info_numeric, x = "WindSpeed",
                     smooth = FALSE, add_hist = TRUE)

shap.plot.dependence(shap_info_numeric, x = "WindSpeed", color_feature = "tvoc_sd",
                     smooth = FALSE, add_hist = TRUE)
shap.plot.dependence(shap_info_numeric, x = "tvoc_sd", color_feature = "WindSpeed",
                     smooth = FALSE, add_hist = TRUE)

augment(xgb_fit_numeric, new_data = baked_data_for_numeric_xgb_w_outcome) |>
  wanted_metrics(truth = benzene, estimate = .pred)
plot_df = augment(xgb_fit_numeric, new_data = baked_data_for_numeric_xgb_w_outcome)
ggplot(plot_df, aes(x = benzene, y = .pred)) +
  geom_point() +
  geom_abline(color = "red") +
  geom_vline(xintercept = log(1)) +
  geom_vline(xintercept = log(10))

tuned_metrics = fit_resamples(xgb_numeric_finalized, rec_spod_numeric, folds_spod_training) |>
  collect_metrics()
tuned_metrics |>
  knitr::kable(digits = 3)

tuned_predictions = fit_resamples(xgb_numeric_finalized, rec_spod_numeric, folds_spod_training,
                                  control = control_resamples(save_pred = TRUE)) |>
  augment()
ggplot(tuned_predictions, aes(x = log(benzene + 0.1), y= .pred)) +
  geom_point() +
  geom_abline(color = "red")

# Ok, this is getting to be where I want it to be.
# I wonder what the best way to resume this later is?
stopCluster(cl)
saveRDS(list(xgb_numeric_finalized = xgb_numeric_finalized,
             xgb_fit_numeric = xgb_fit_numeric,
             rec_spod_numeric = rec_spod_numeric,
             data_spod_training = data_spod_training,
             data_spod_testing = data_spod_testing,
             folds_spod_training = folds_spod_training),
        "xgboost_numeric_model_info.RDS")

### Point of comparison: lm + splines ----
rec_spod_spline = rec_spod_numeric |>
  step_ns(tvoc_sd, WindSpeed, deg_free = 6)

lm_fit_spline =
  fit_resamples(lm_mod, rec_spod_spline, resamples = folds_spod_training,
                control = control_resamples(save_pred = TRUE))
lm_fit_spline |>
  collect_metrics() |>
  knitr::kable(digits = 2)
# I"m getting an rmse of 0.529 and an r2 of 0.359 - against, that's better than I was expecting
# for out-of-sample error, and a lot better than we were getting without the splines.

predictions = lm_fit_spline |>
  collect_predictions(summarize = TRUE)

plot_df = augment(lm_fit_spline)
ggplot(plot_df, aes(x = tvoc_sd, y = .pred)) +
  geom_point() +
  scale_x_log10()

ggplot(plot_df, aes(x = WindSpeed, y = .pred)) +
  geom_point()

ggplot(plot_df, aes(x = log(benzene + 0.1), y= .pred)) +
  geom_point()  +
  geom_abline(color = "red")
