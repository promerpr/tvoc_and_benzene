library(tidyverse)
library(rsample)
library(lubridate)
library(recipes)
library(parsnip)
library(workflows)

# Our goal here is to take what we learned from our first approach and start again, taking into
# account the different deployments.

# I think there are some risks here with data leakage, but I think this is more important to do.
data_for_modeling = read_csv("parsed_data/camml/camml_combined_data.csv")

# The flags we should treat as a factor
data_for_modeling = data_for_modeling |>
  mutate(flags = factor(flags),
         flags = fct_explicit_na(flags)) |>
  mutate(operational_phase = fct_explicit_na(operational_phase)) |>
  mutate(source_direction_degrees = as.numeric(source_direction_degrees)) |>
  replace_na(list(source_direction_degrees = 0))

data_for_modeling |>
  mutate(across(everything(), is.na)) |>
  count(across(everything()), sort = TRUE) |>
  print(width = Inf)
# Hrm.
# I think we should try to do 2 different approaches...but let's do the split on the complete
# cases
deployments = data_for_modeling |>
  na.omit() |>
  distinct(deployment_id, deployment_name)
set.seed(1435235)
deployment_split = initial_split(deployments, prop = 0.8)
deployment_training = training(deployment_split)
deployment_testing = testing(deployment_split)

data_training = semi_join(data_for_modeling, deployment_training)
data_training_naomit = data_training |>
  filter(if_all(.cols = -c(O3, CO2, NOx, NO, NO2, `PM2.5(LTP)`, `PM10(LTP)`, `T(shelter)`, CH4, SolarRadiation),
                \(x) !is.na(x))) |>
  filter(benzene > 0)
data_training_naomit
data_testing = semi_join(data_for_modeling, deployment_testing)

# Let's make a version without any NA's. Where do we see NAs?
library(naniar)
gg_miss_var(data_training_naomit)
# We now have our training and testing data.
# So let's work on some models!

## Model with no deployment-specific information----
tvoc_rec = recipe(benzene ~ ., data = data_training) |>
  update_role(end_time_lst, gc_id, gc_end_time, new_role = "ID") |>
  update_role(deployment_id, deployment_name, pad_name, pad_lat, pad_lon,
              camml_lat, camml_lon, operational_phase, distance_to_source_ft,
              source_direction_degrees, new_role = "ID") |>
  step_log(benzene) |>
  step_naomit(everything()) |>
  step_date(start_time_lst, features = c("dow", "month")) |>
  step_time(start_time_lst, features = "hour", keep_original_cols = FALSE) |>
  step_nzv(all_nominal_predictors())
# See if this makes sense:
prep(tvoc_rec, data = data_training) |>
  bake(new_data = NULL)

# Fit our model to this version
tvoc_mod = parsnip::rand_forest(trees = 1000, mtry = 6) |>
  set_mode("regression") |>
  set_engine("ranger")

tvoc_flow = workflow() |>
  add_recipe(tvoc_rec) |>
  add_model(tvoc_mod)

tvoc_fit = fit(tvoc_flow, data = data_training)
tvoc_fit

# How much are our other meteorological variables influencing the results?
tvoc_rec_nochem = tvoc_rec |>
  step_rm(`PM2.5(LTP)`, `PM10(LTP)`, CH4, NO, NO2, NOx, O3, `T(shelter)`, CO2, u, v, SolarRadiation)
tvoc_flow_no_chem = workflow() |>
  add_recipe(tvoc_rec_nochem) |>
  add_model(tvoc_mod)
tvoc_fit_nochem = fit(tvoc_flow_no_chem, data = data_training)
tvoc_fit_nochem
# They were actually pretty useful. Which makes sense - I'd expect information about some of the variables to tell us something.
# Are NA's in numeric predictors something that random forests can handle? I don't know. What about coding it as a negative value slash setting a flag? My understanding is that's something that the setup should be able to handle.

library(naniar)
miss_var_summary(data_training) |>
  print(n = Inf)
# OK, looks like random forests are able to handle this.
# Which implies that I should just try to include everything? Or I could remove the biggest things?
# Or do we want S-POD + Met only, to kinda replicate the information that would be collected when
# only an SPOD is deployed?
# Probably closer to the second one.
# So, what information is collected by an SPOD?
# TVOC, Temp, Pressure, RH, Wind Speed, and Wind Direction


# What variables are important in our data?
tvoc_fit$fit$fit$fit$treetype


library(vip)
# vip(tvoc_fit)
tvoc_mod2 = parsnip::rand_forest(trees = 1000, mtry = 6) |>
  set_mode("regression") |>
  set_engine("ranger", importance = "permutation")
tvoc_fit2 = workflow() |>
  add_recipe(tvoc_rec_nochem) |>
  add_model(tvoc_mod2) |>
  fit(data = data_training)
# vip(tvoc_fit2, geom = "point")
res = fit(tvoc_mod2, formula = benzene ~ ., data = data_training)
vip(res, geom = "point")
# Really annoying that you can't use vip on workflows. I'm thinkging that workflows are
# probably not worth it...except maybe they are for tuning? I'm not sure.
# But let's maybe not worry about workflows and focus on recipes and parsnip for now.


###

# So anyway, let's try to do some proper resampling approaches.
# I'm going to want to be doing the resampling at the deployment level - I'm hoping that
# will give me some more information that I'm looking at.

camml_folds = group_vfold_cv(data_training, group = deployment_id)
camml_folds

camml_folds_naomit = group_vfold_cv(data_training_naomit, group = deployment_id)
camml_folds_naomit
# deployment


# Now let's set up our recipe to only use the parameters that would be available with an S-POD
# I'm having trouble getting the na.omit steps to work, so I'm going to remove thrm from the
# entire set ahead of time.
tvoc_rec_spod = recipe(benzene ~ ., data = data_training_naomit) |>
  update_role(end_time_lst, gc_id, gc_end_time, new_role = "ID") |>
  update_role(deployment_id, deployment_name, pad_name, pad_lat, pad_lon,
              camml_lat, camml_lon, operational_phase, distance_to_source_ft,
              source_direction_degrees, new_role = "ID") |>
  step_log(benzene) |>
  step_date(start_time_lst, features = c("dow", "month")) |>
  step_time(start_time_lst, features = "hour", keep_original_cols = FALSE) |>
  step_nzv(all_nominal_predictors()) |>
  step_rm(`PM2.5(LTP)`, `PM10(LTP)`, CH4, NO, NO2, NOx, O3, `T(shelter)`, CO2, u, v, SolarRadiation)
test_data = prep(tvoc_rec_spod, data = data_training_naomit) |>
  bake(new_data = data_training_naomit, all_predictors(), all_outcomes())
print(test_data, width = Inf)
test_data |>
  count(is.na(WindSpeed))
# That's actually fairly limited - 12 predictor variables.

library(tune)
tvoc_resample_fit1 = fit_resamples(tvoc_mod2, tvoc_rec_spod, resamples = camml_folds_naomit,
              control = control_resamples(save_pred = TRUE))
tvoc_resample_fit1
tvoc_resample_fit1$.notes
collect_metrics(tvoc_resample_fit1)
# When we measure for a different deployment, we get much worse results (as we'd expect)
tvoc_resample_pred = collect_predictions(tvoc_resample_fit1, summarize = TRUE)
ggplot(tvoc_resample_pred, aes(x = benzene, y= .pred)) +
  geom_point() +
  geom_abline()
# And it's actually even worse than that! I think our R2 is only as big as it is
# because there's a cluster of benzene measurements that are very low - maybe there was
# one deployment with an especially low detection limit?

## Looking into Benzene measurements
ggplot(data_training, aes(x = log10(benzene))) +
  geom_histogram()
# Actually, it looks pretty smooth. I think I'm picking up on the sharp divide in the predictions,
# when actually the benzene concentrations are pretty smooth.

# So, we've got a starting model...it's honestly not great. But I'm set up that I can try to add
# in some deployment-specific predictors, and we can see if that improves or degrades the model.

sd(log(data_training_naomit$benzene))
# Right, so an R2 of 0.38 means that our measured data can explain about 38% of the variance.
# That's ... well, what R2 would we need before we started to think this data was useful?

# Next steps? ----
# 1. Add in deployment-specific information (distance to pad, AirToxScreen benzene concentration,
#     ATC np_oil_gas benzene, ...what else? )
# 2. Add in more TVOC-data - either more summary statistics or like 60 different TVOC columns?
# 3. Should I regularize the TVOC columns? Best practice would say yes, but it feels strange to me.


#  operational_phase, distance_to_source_ft, source_direction_degrees) |>
tvoc_rec_spod_w_dep_info = recipe(benzene ~ ., data = data_training_naomit) |>
  update_role(end_time_lst, gc_id, gc_end_time, new_role = "ID") |>
  update_role(deployment_id, deployment_name, pad_name, pad_lat, pad_lon,
              camml_lat, camml_lon, new_role = "ID") |>
  step_log(benzene) |>
  step_date(start_time_lst, features = c("dow", "month")) |>
  step_time(start_time_lst, features = "hour", keep_original_cols = FALSE) |>
  step_nzv(all_nominal_predictors()) |>
  step_rm(`PM2.5(LTP)`, `PM10(LTP)`, CH4, NO, NO2, NOx, O3, `T(shelter)`, CO2, u, v, SolarRadiation) |>
  step_mutate(wind_offset = abs(WindDirection - source_direction_degrees)) #TODO(): Check that my wind directions are correct.
# This is the same as tvoc_rec_spod, but I'm adding in information about the operational phase, and the distance and the source direction.
# I wonder if wind direciton I could also pivot out wider?
prep(tvoc_rec_spod_w_dep_info, data = data_training_naomit) |>
  bake(new_data = NULL)
count(data_training_naomit, deployment_name, distance_to_source_ft)
count(data_training_naomit, deployment_name, source_direction_degrees)
count(data_training_naomit, deployment_name, operational_phase)
count(data_training_naomit, operational_phase) #Good behavoir there.

# Now let's just fit the model and see
tvoc_resample_fit2 = fit_resamples(tvoc_mod2, tvoc_rec_spod_w_dep_info, resamples = camml_folds_naomit,
                                   control = control_resamples(save_pred = TRUE))
tvoc_resample_fit2
tvoc_resample_fit2$.notes

collect_metrics(tvoc_resample_fit2)
# When we measure for a different deployment, we get much worse results (as we'd expect)
tvoc_resample_pred2 = collect_predictions(tvoc_resample_fit2, summarize = TRUE)
ggplot(tvoc_resample_pred2, aes(x = benzene, y= .pred)) +
  geom_point() +
  geom_abline() +
  geom_smooth(method= "lm")
# Yeah, that doesn't look great.
# Let's try to pull in some data from ATC?
# Let's use the 2017 ATC, since we don't have other years.


atc_conc_2017 = read_csv("../AirToxScreen/parsed_data_2017/co_concentration_by_sector.csv")
library(sf)
co_ct = st_read("X:/Shared drives/_CDPHE TEEO Data/Geospatial Boundaries/colorado_census_tracts/colorado_census_tracts.shp")
co_ct
atc_benzene = atc_conc_2017 |>
  filter(pollutant_name == "BENZENE") |>
  filter(sector %in% c("total", "np_oil_gas"))  |>
  select(tract, sector, ambient_conc) |>
  pivot_wider(names_from = sector, names_prefix = "atc_benzene_", values_from = ambient_conc)

# Go back to the starting data to get this set up properly:
data_training_naomit2 = data_training_naomit |>
  st_as_sf(coords = c("camml_lon", "camml_lat"), crs = "+proj=longlat", remove = FALSE) |>
  st_join(co_ct) |>
  st_drop_geometry() |>
  select(-countyfips, -statefips)  |>
  left_join(atc_benzene, by = c(tractfips = 'tract'))
waldo::compare(data_training_naomit, data_training_naomit2)

# Ok, let's see if that helped much at all?
camml_folds_naomit2 = group_vfold_cv(data_training_naomit2, group = deployment_id)
camml_folds_naomit2
# data_training = semi_join(data_for_modeling, deployment_training)
# data_training_naomit = data_training |>
  # filter(if_all(.cols = -c(O3, CO2, NOx, NO, NO2, `PM2.5(LTP)`, `PM10(LTP)`, `T(shelter)`, CH4, SolarRadiation),
                # \(x) !is.na(x))) |>
  # filter(benzene > 0)
# data_training_naomit
# data_testing = semi_join(data_for_modeling, deployment_testing)
tvoc_rec_spod_w_dep_info_and_atc = recipe(benzene ~ ., data = data_training_naomit2) |>
  update_role(end_time_lst, gc_id, gc_end_time, new_role = "ID") |>
  update_role(deployment_id, deployment_name, pad_name, pad_lat, pad_lon,
              camml_lat, camml_lon, tractfips, new_role = "ID") |>
  step_log(benzene) |>
  step_date(start_time_lst, features = c("dow", "month")) |>
  step_time(start_time_lst, features = "hour", keep_original_cols = FALSE) |>
  step_nzv(all_nominal_predictors()) |>
  step_rm(`PM2.5(LTP)`, `PM10(LTP)`, CH4, NO, NO2, NOx, O3, `T(shelter)`, CO2, u, v, SolarRadiation) |>
  step_mutate(wind_offset = abs(WindDirection - source_direction_degrees))
tvoc_resample_fit3 = fit_resamples(tvoc_mod2, tvoc_rec_spod_w_dep_info_and_atc,
                                   resamples = camml_folds_naomit2,
                                   control = control_resamples(save_pred = TRUE))
tvoc_resample_fit3
tvoc_resample_fit3$.notes

collect_metrics(tvoc_resample_fit3)
# That didn't seem to help at all
tvoc_resample_pred3 = collect_predictions(tvoc_resample_fit3, summarize = TRUE)
ggplot(tvoc_resample_pred3, aes(x = benzene, y= .pred)) +
  geom_point() +
  geom_abline() +
  geom_smooth(method= "lm")
# Ok, so the ATC concentration's didn't seem to help.
# Let's seem if they're showing up in VIP at all?
baked_data_for_vip = prep(tvoc_rec_spod_w_dep_info_and_atc, data = data_training_naomit2) |>
  bake(new_data = NULL, all_predictors(), all_outcomes())
res = fit(tvoc_mod2, formula = benzene ~ ., data = baked_data_for_vip)
vip(res, geom = "point")
# Hrm. Flags are very predictive - that's probably an error on my part, since the flags aren't something that we would know in advance.

### Dealing with flags ----
data_training_naomit2 |>
  count(flags)
data_training_naomit2 |>
  filter(flags == "4")
ggplot(data_training_naomit2, aes(x = log(benzene), fill = flags)) +
  geom_histogram()
# Yeah, OK, the flags variable is probably explaining the sharp divide I was seeing in my predictions
# earlier. So we should definitely deal with that.

# So let's make a new recipe:
tvoc_rec_spod_v4 = recipe(benzene ~ ., data = data_training_naomit2) |>
  update_role(end_time_lst, gc_id, gc_end_time, new_role = "ID") |>
  update_role(deployment_id, deployment_name, pad_name, pad_lat, pad_lon,
              camml_lat, camml_lon, tractfips, new_role = "ID") |>
  step_log(benzene) |>
  step_date(start_time_lst, features = c("dow", "month")) |>
  step_time(start_time_lst, features = "hour", keep_original_cols = FALSE) |>
  step_rm(flags) |> # This is more of an outcome than a predictor
  step_nzv(all_nominal_predictors()) |>
  step_rm(`PM2.5(LTP)`, `PM10(LTP)`, CH4, NO, NO2, NOx, O3, `T(shelter)`, CO2, u, v, SolarRadiation) |>
  step_mutate(wind_offset = abs(WindDirection - source_direction_degrees))


tvoc_resample_fit4 = fit_resamples(tvoc_mod2, tvoc_rec_spod_v4,
                                   resamples = camml_folds_naomit2,
                                   control = control_resamples(save_pred = TRUE))
tvoc_resample_fit4
tvoc_resample_fit4$.notes

collect_metrics(tvoc_resample_fit4)

tvoc_resample_pred4 = collect_predictions(tvoc_resample_fit4, summarize = TRUE)
ggplot(tvoc_resample_pred4, aes(x = benzene, y= .pred)) +
  geom_point() +
  geom_abline() +
  geom_smooth(method= "lm")
baked_data_for_vip = prep(tvoc_rec_spod_v4, data = data_training_naomit2) |>
  bake(new_data = NULL, all_predictors(), all_outcomes())
res = fit(tvoc_mod2, formula = benzene ~ ., data = baked_data_for_vip)
vip(res, geom = "point")
# Wind offset and windSpeed are showing up as the most important variables, which is really interesting.

# And TVOC is showing up, but it's pretty low down the list.

# Ok, last thing I want to look at:
# What happens if we look at 10- or 5-minute chunks of TVOC and wind direction?
wide_camml_tvoc_wd_raw = read_csv("parsed_data/camml/camml_wide_spod_wind.csv")
wide_camml_tvoc_wd = wide_camml_tvoc_wd_raw |>
  mutate(across(starts_with("WindDirection"), \(x) 270 - x*360/(2*pi))) |>
  mutate(across(starts_with("WindDirection"), \(x) ifelse(x > 360, x - 360, x))) |>
  na.omit()
data_training_naomit3 = data_training_naomit2 |>
  inner_join(wide_camml_tvoc_wd, by = "gc_id") |>
  mutate(across(starts_with("WindDirection_"), \(x) abs(x - source_direction_degrees)))
camml_folds_naomit3 = group_vfold_cv(data_training_naomit3, group = deployment_id)

tvoc_rec_spod_v5 = recipe(benzene ~ ., data = data_training_naomit3) |>
  update_role(end_time_lst, gc_id, gc_end_time, new_role = "ID") |>
  update_role(deployment_id, deployment_name, pad_name, pad_lat, pad_lon,
              camml_lat, camml_lon, tractfips, new_role = "ID") |>
  step_log(benzene) |>
  step_date(start_time_lst, features = c("dow", "month")) |>
  step_time(start_time_lst, features = "hour", keep_original_cols = FALSE) |>
  step_rm(flags) |> # This is more of an outcome than a predictor
  step_nzv(all_nominal_predictors()) |>
  step_rm(`PM2.5(LTP)`, `PM10(LTP)`, CH4, NO, NO2, NOx, O3, `T(shelter)`, CO2, u, v, SolarRadiation) #|>
  # step_mutate(wind_offset = abs(WindDirection - source_direction_degrees)) # This is now done in the data prep step

# Check that we have the columns we think we have:
prep(tvoc_rec_spod_v5, data = data_training_naomit3) |>
  bake(new_data = NULL, all_predictors(), all_outcomes())
# Looks reasonable to me

tvoc_resample_fit5 = fit_resamples(tvoc_mod2, tvoc_rec_spod_v5,
                                   resamples = camml_folds_naomit3,
                                   control = control_resamples(save_pred = TRUE))
tvoc_resample_fit5
tvoc_resample_fit5$.notes

collect_metrics(tvoc_resample_fit4)
collect_metrics(tvoc_resample_fit5)
# We're doing a little better, but not by much.

tvoc_resample_pred5 = collect_predictions(tvoc_resample_fit5, summarize = TRUE)
ggplot(tvoc_resample_pred5, aes(x = benzene, y= .pred)) +
  geom_point() +
  geom_abline() +
  geom_smooth(method= "lm")
baked_data_for_vip = prep(tvoc_rec_spod_v5, data = data_training_naomit3) |>
  bake(new_data = NULL, all_predictors(), all_outcomes())
res = fit(tvoc_mod2, formula = benzene ~ ., data = baked_data_for_vip)
vip(res, geom = "point", num_features = 20)

# Hrm. Not much improvement from the 18 added variables. What if we...
# 1. Corrected the wind direction
# 2. Added some lagged tvoc variables? Maybe also lagged T, P, and RH?

data_training_naomit3
library(slider)
data_training_naomit4 = data_training_naomit3 |>
  group_by(deployment_id) |>
  mutate(across(c(tvoc, RH, `T(ambient)`, `P(barometric)`),
                .fns = list(day = \(x) slide_index_dbl(tvoc, start_time_lst, mean, .before = days(1), .complete = FALSE),
                            week = \(x) slide_index_dbl(tvoc, start_time_lst, mean, .before = days(7), .complete = FALSE)
                            ))) |>
  ungroup() |>
  mutate(WindDirection =  270 - WindDirection*360/(2*pi)) |>
  mutate(WindDirection = ifelse(WindDirection > 360, WindDirection - 360, WindDirection)) |>
  mutate(WindDirection = abs(WindDirection - source_direction_degrees)) |>
  mutate(across(starts_with("WindDirection"), \(x) ifelse(x > 180, 360-x, x)))
camml_folds_naomit4 = group_vfold_cv(data_training_naomit4, group = deployment_id)

tvoc_rec_spod_v6 = recipe(benzene ~ ., data = data_training_naomit4) |>
  update_role(end_time_lst, gc_id, gc_end_time, new_role = "ID") |>
  update_role(deployment_id, deployment_name, pad_name, pad_lat, pad_lon,
              camml_lat, camml_lon, tractfips, new_role = "ID") |>
  step_log(benzene) |>
  step_date(start_time_lst, features = c("dow", "month")) |>
  step_time(start_time_lst, features = "hour", keep_original_cols = FALSE) |>
  step_rm(flags) |> # This is more of an outcome than a predictor
  step_nzv(all_nominal_predictors()) |>
  step_rm(`PM2.5(LTP)`, `PM10(LTP)`, CH4, NO, NO2, NOx, O3, `T(shelter)`, CO2, u, v, SolarRadiation)

# Check that we have the columns we think we have:
prep(tvoc_rec_spod_v6, data = data_training_naomit4) |>
  bake(new_data = NULL, all_predictors(), all_outcomes())
# Looks reasonable to me

# library(doParallel)
# c1 = makePSOCKcluster(parallel::detectCores(logical = FALSE))
# registerDoParallel(c1)
tvoc_resample_fit6 = fit_resamples(tvoc_mod2, tvoc_rec_spod_v6,
                                   resamples = camml_folds_naomit4,
                                   control = control_resamples(save_pred = TRUE))
# stopCluster(c1)
tvoc_resample_fit6
tvoc_resample_fit6$.notes

collect_metrics(tvoc_resample_fit4)
collect_metrics(tvoc_resample_fit5)
collect_metrics(tvoc_resample_fit6)
# That actually makes things slightly worse. Oops!

tvoc_resample_pred6 = collect_predictions(tvoc_resample_fit6, summarize = TRUE)
ggplot(tvoc_resample_pred6, aes(x = benzene, y= .pred)) +
  geom_point() +
  geom_abline() +
  geom_smooth(method= "lm")
baked_data_for_vip = prep(tvoc_rec_spod_v6, data = data_training_naomit4) |>
  bake(new_data = NULL, all_predictors(), all_outcomes())
res = fit(tvoc_mod2, formula = benzene ~ ., data = baked_data_for_vip)
vip(res, geom = "point", num_features = 20)
hist(baked_data_for_vip$WindSpeed)

ggplot(data_training_naomit4, aes(x = WindSpeed, y = benzene)) +
  geom_point() +
  scale_y_log10()
# Interesting. That is a fairly significant relationship - like I can see that there's a shape.
ggplot(data_for_modeling, aes(x = tvoc, y = benzene)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()
# I cannot see an obvious relationship

ggplot(data_training_naomit4, aes(x = WindSpeed, y = benzene, color = log10(tvoc))) +
  geom_point() +
  scale_y_log10()
  scale_y_log10()

  data_training_naomit4 |>
    count(tvoc < 0, tvoc == 0)
data_training_naomit4 |>
  filter(tvoc <= 0)

hist(data_training_naomit4$tvoc)
ggplot(data_training_naomit, aes(x = `T(ambient)`, y = tvoc)) +
  geom_point()

# Well, anyway, that's probably not useful.
# Can we work on tuning the model?

tvoc_mod3 = parsnip::rand_forest(trees = 1000, mtry = tune(), min_n = tune()) |>
  set_mode("regression") |>
  set_engine("ranger", importance = "permutation") #Removing tree depth because I don't know how to make it work

library(doParallel)
c1 = makePSOCKcluster(parallel::detectCores(logical = FALSE))
registerDoParallel(c1)
set.seed(1542385)
ranger_tune = tune_grid(tvoc_mod3, tvoc_rec_spod_v6, resamples = camml_folds_naomit4, grid = 15)
stopCluster(c1)
autoplot(ranger_tune)
collect_metrics(ranger_tune) |>
  filter(.metric == "rmse") |>
  arrange(desc(mean))

tvoc_mod4 = parsnip::rand_forest(trees = 1000, mtry = 19) |>
  set_mode("regression") |>
  set_engine("ranger", importance = "permutation")
tvoc_resample_fit7 = fit_resamples(tvoc_mod4, tvoc_rec_spod_v6, resamples = camml_folds_naomit4)
collect_metrics(tvoc_resample_fit7)
collect_metrics(tvoc_resample_fit6)
# Well, we're not going any better.
# Alright, I think we've probably reached the end of what we can reasonably accomplish here.
# I think that maybe it's time to move into a new document, do a little more targeted feature engineering,
# a tuning run, and then call it good.
# I should also do some data exploration on the timeseries of TVOCs and the distribution of TVOC
# measurements by deployment.

