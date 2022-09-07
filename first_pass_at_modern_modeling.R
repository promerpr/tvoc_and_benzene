set.seed(960194668)

library(tidyverse)
library(rsample)

camml_voc = read_csv("parsed_data/camml/camml_voc_data_gcid.csv")
camml_other = read_csv("parsed_data/camml/camml_other_data_gcid.csv")

n_pid = camml_other  |>
  filter(measurement == "VOC(pid)") |>
  filter(!is.na(gc_id)) |>
  group_by(gc_id) |>
  summarize(tvoc = mean(value, na.rm = TRUE),
            n = sum(is.finite(value)))

good_data_points = camml_voc  |>
  left_join(n_pid, by = "gc_id") |>
  filter(voc == "benzene") |>
  filter(n >= 25)

camml_split = initial_split(good_data_points, prop = 0.8, strata = concentration)
camml_train = training(camml_split)
camml_train
ggplot(camml_train, aes(x = concentration)) +
  geom_histogram()
# Definitely not normal; I'd probably like to log-transform these.
ggplot(camml_train, aes(x = concentration)) +
  geom_histogram() +
  scale_x_log10()
# That's a lot closer to being normally distributed. Which I think is going to be nice for us.

# Ok, there's probbly some complicated recipe that I should be using, but let's start by
# doing it manually using the randomForest and ranger packages
# For now, I'm adapating code from https://towardsdatascience.com/data-science-tutorials-training-a-random-forest-in-r-a883cc1bacd1

df = camml_train |>
  mutate(log_conc = log(concentration)) |>
  filter(!is.na(log_conc))
df
library(randomForest)
# rf <- randomForest(formula = log_conc ~ . ,
#                    data = df,
#                    ntree = 100)
# # This is giving me errors complaining about missing variables. So let's try to clean it up
df2 = df |>
  select(-flags) |>
  na.omit()
rf <- randomForest(formula = log_conc ~ . ,
                   data = df2,
                   ntree = 100)
rf$importance
# Oh right, we left in concentration, so it can just pull that.
df3 = df2 |>
  select(-concentration)
rf3 <- randomForest(formula = log_conc ~ . ,
                   data = df3,
                   ntree = 100)
rf3
rf3$importance
plot(df3$log_conc, rf3$predicted)
# That actually seems pretty good. That's fun to see. I still don't understand what a random forest
# is though.

# Can we pull out a single tree?
getTree(rf3, k = 1, labelVar = TRUE) |>
  count(`split var`)
# So looks like they do binary splits; we reuse classes.
# This tree has 14 different variables
getTree(rf3, k = 14, labelVar = TRUE) |>
  count(`split var`)

# What happens as we increase the number of trees?
rf3_1000 = randomForest(formula = log_conc ~ . ,
                        data = df3,
                        ntree = 1000)
rf3_1000

rf3_50 = randomForest(formula = log_conc ~ . ,
                      data = df3,
                      ntree = 50)
rf3_50

rf3_10 = randomForest(formula = log_conc ~ . ,
                      data = df3,
                      ntree = 10)
rf3_10

# Really not as much difference as I was expected. But maybe with fewer trees you start to
# worry about overfitting? And so we really just want to get good coverage there, and that's
# why you'd increase the number of trees.

# Now let's see what happens if we use the ranger package:
library(ranger)
ranger1 = ranger(formula = log_conc ~ . ,
                 data = df3,
                 num.trees = 100)
ranger1
# I'm getting a much higher MSE and a lower R2. I wonder if that's because of the number of
# variables tried at each split is lower? Let's try adjusting that and seeing
ranger2 = ranger(formula = log_conc ~ .,
                 data = df3,
                 num.trees = 100,
                 mtry = \(x) x/3,
                 importance = "impurity") # Not sure which of these to use
ranger2
# Yeah, that's a lot closer to what I was seeing for the randomForest results.
ranger2_1000 = ranger(formula = log_conc ~ .,
                      data = df3,
                      num.trees = 1000,
                      mtry = \(x) x/3)
ranger2_1000
# These are basically fitting instantaneously, which is cool.
plot(df3$log_conc, ranger2$predictions)
ranger2$forest
ranger2$variable.importance
treeInfo(ranger2, tree = 1) |>
  count(terminal)
706*5
# Looks fairly similar to the randomForest models.

## Now the two things I want to do next are:
# 1. Switch to using more of a tidymodels workflow
# 2. Do some feature engineering to get some more useful data for me:
#   a. hour, dow, and month
#   b. Wind speed and direction (direction relative to the source location)
#   c. More variables related to the tvoc measurements?
#   d. Other meteorological variables.

library(recipes)
tvoc_rec = recipe(log_conc ~ ., data = df3) |>
  update_role(end_time_lst, gc_id, gc_end_time, new_role = "ID") |>
  step_date(start_time_lst, features = c("dow", "month")) |>
  step_time(start_time_lst, features = "hour", keep_original_cols = FALSE)
library(parsnip)
tvoc_mod = parsnip::rand_forest(trees = 1000, mtry = 6) |>
  set_mode("regression") |>
  set_engine("ranger")
library(workflows)
tvoc_flow = workflow() |>
  add_recipe(tvoc_rec) |>
  add_model(tvoc_mod)
tvoc_flow
tvoc_fit = fit(tvoc_flow, data = df3)
tvoc_fit
prep(tvoc_rec, data = df3) |>
  bake(new_data = df3)

# Ok, that's reasonable. I think we should do some more pre-processing before we do the split though.
# camml_voc = read_csv("parsed_data/camml/camml_voc_data_gcid.csv")
# camml_other = read_csv("parsed_data/camml/camml_other_data_gcid.csv")
# First thing is to get our different measurements pivotted wider
# But before we do that, we have to clean up the units
# I'm just going to do this manually
camml_other |>
  count(measurement, units)

camml_other_units_fixed = camml_other |>
  mutate(value = case_when(measurement == "O3" & units == "ppmV" ~ value * 1000,
                           measurement == "VOC(pid)" & units == "IBeq_ppbV" ~ value,
                           TRUE ~ value),
         units = case_when(measurement == "O3" & units == "ppmV" ~ "ppbV",
                           measurement == "VOC(pid)" & units == "IBeq_ppbV" ~ "ppbV",
                           TRUE ~ units))
camml_other_units_fixed |>
  count(measurement, units)
# That looks right

# Check that we've uniquely identified the rows:
library(lubridate)
camml_other_units_fixed |>
  filter(measurement == "RH") |>
  group_by(gc_id, start_time_lst, end_time_lst, gc_start_time ) |>
  summarize(n = n(), .groups = "drop") |>
  filter(n > 1) |>
  count(floor_date(gc_start_time, "day")) |>
  print(n = Inf)
# All rows are uniquely identified

camml_other_wide = camml_other_units_fixed |>
  select(gc_id, gc_deployment_id, start_time_lst, end_time_lst, measurement, units, value) |>
  pivot_wider(id_cols = c(gc_id, gc_deployment_id, start_time_lst, end_time_lst),
              names_from = measurement,
              values_from = value)
# Now we're going to have to average down. I'll need to convert the wind speed and direction
# to u and v in order to average properLY

camml_other_aligned = camml_other_wide |>
  mutate(math_winddir_rad = 2*pi * (270 - WindDirection)/360,
         u = WindSpeed * cos(math_winddir_rad),
         v = WindSpeed * sin(math_winddir_rad)) |>
  select(-WindSpeed, -WindDirection, -math_winddir_rad) |>
  rename(tvoc = `VOC(pid)`) |>
  group_by(gc_id) |>
  summarize(n = sum(is.finite(tvoc)),
            across(RH:v, mean, na.rm = TRUE),
            tvoc_low = quantile(tvoc, 0.05, na.rm = TRUE),
            tvoc_high = quantile(tvoc, 0.95, na.rm = TRUE))
camml_other_aligned = camml_other_aligned |>
  mutate(WindSpeed = sqrt(u^2 + v^2),
         WindDirection = atan2(v, u))

# n_pid = camml_other  |>
#   filter(measurement == "VOC(pid)") |>
#   filter(!is.na(gc_id)) |>
#   group_by(gc_id) |>
#   summarize(tvoc = mean(value, na.rm = TRUE),
#             n = sum(is.finite(value)))
combined_camml_data = camml_voc |>
  filter(voc == "benzene") |>
  left_join(camml_other_aligned, by = "gc_id")
combined_camml_data |>
  count(n, sort = TRUE)
data_for_modeling = combined_camml_data |>
  filter(n >= 25) |>
  pivot_wider(names_from = voc, values_from = concentration)

# I'm getting to be a little concerned about over-fitting on the deployments. Presumably each
# deployment has its own expected level of benzene and I'm worried the model is picking up on that,
# which isn't going to be applicable is we deploy in a new place.

## First things first - do we actually see big variation between deployments?
ggplot(data_for_modeling, aes(x = deployment_name, y = benzene)) +
  geom_jitter(size = 0.1, width = 0.25)+
  geom_violin(alpha = 0, fill = "blue") +
  scale_y_log10() +
  coord_flip()

# I'd say we see a ... medium about of variation between deployments?
# That definitely does seem like something we should be paying attention to.
# But at the same time there is some useful information in the deployments - i.e., I would expect
# the relationship to be different in areas that are closer to the oil & gas pads than in areas
# that are further away. But I don't know how to incorprate that information without overfitting?
#
# I don't know the best approach for machine learning w/ these hierarchical models.
# But two options that are coming to mind are:
# 1. Throw out all deployment-specific information (absolute date, deployment, lat, lon, phase?, distance to source, source direction)
# 2. Do a cross-validation /test set where we explicitly leave out a deployment and we see how it did.
# I think that trid one will be very important. So I think we should do a second pass with this.


