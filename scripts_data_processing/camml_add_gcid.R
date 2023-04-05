library(tidyverse)
library(lubridate)
library(progress)
library(naniar)


camml_voc = read_csv("parsed_data/camml/camml_voc_data.csv")
camml_other_wide = read_csv("parsed_data/camml/camml_other_data_wide.csv")

camml_voc
camml_other_wide

# First, come up with our gc_id's
camml_gc_runs = camml_voc |>
  distinct(gc_start_time = start_time_lst, gc_end_time = end_time_lst,
           gc_deployment_id = deployment_id) |>
  arrange(gc_start_time) |>
  filter(!is.na(gc_start_time), !is.na(gc_end_time)) |>
  mutate(gc_id = row_number())
plot(camml_gc_runs$gc_id, camml_gc_runs$gc_start_time)
plot(camml_gc_runs$gc_id, camml_gc_runs$gc_deployment_id)
# That looks about what I'd expect - periods with a consistent slope with some jumps in between them
# between deployments.

# We're just going to do this manually with a for loop, rather than trying to be fancy with
# data.table and roll joins.
# (We could now do this with dplyr and join_by, but I think it's easier to keep the old approach)
camml_other_wide_gcid = camml_other_wide |>
  mutate(gc_id = NA_integer_)
pb <- progress_bar$new(total = nrow(camml_gc_runs))
for (i in seq_len(nrow(camml_gc_runs))) {
  m =
    camml_other_wide_gcid$start_time_lst >= camml_gc_runs$gc_start_time[[i]] &
    camml_other_wide_gcid$start_time_lst < camml_gc_runs$gc_end_time[[i]] &
    camml_other_wide_gcid$deployment_id == camml_gc_runs$gc_deployment_id[[i]]
  if (any(!is.na(camml_other_wide_gcid$gc_id[m]))) {
    message("SPOD measurement associated with 2 gc measurements!")
    browser()
  }
  camml_other_wide_gcid$gc_id[m] = camml_gc_runs$gc_id[[i]]
  pb$tick()
}

# Check to make sure that we did this correctly
camml_other_wide_gcid |>
  filter(deployment_id == 2) |>
  select(start_time_lst, deployment_id, gc_id) |>
  print(n = 75)
# Visually, it looks like we go through periods where the tvoc measurements have a gc_id and
# periods when they don't

ggplot(camml_other_wide, aes(x = start_time_lst, y = deployment_id)) +
  geom_point() +
  geom_point(data = camml_gc_runs, aes(x = gc_start_time, y= gc_deployment_id+0.2), color = 'blue',
             size = 0.9)

ggplot(camml_other_wide_gcid, aes(x = start_time_lst, y = gc_id)) +
  geom_miss_point()
# There are a lot of points without a matching gc_id


camml_other_wide_gcid |>
  count(has_gcid = is.finite(gc_id)) |>
  mutate(pcnt = n/sum(n))
# About 55% of the measurements are matched with a gc_id

camml_gc_runs |>
  summarize(med_length = median(gc_end_time - gc_start_time),
            med_takt = median(gc_start_time - lag(gc_start_time, 1), na.rm = TRUE))
# The gc is operating for 40-45 minutes out of every hour, so just based on that we'd expect
# 60 - 75% of points to be matched.

# Add in some oddities - like deployment 14 and 15, and it's reasonable to see how you could get
# down to 55%.
camml_gc_runs |>
  group_by(gc_deployment_id) |>
  mutate(lag_start = lag(gc_start_time, 1)) |>
  ungroup() |>
  summarize(avg_length = mean(gc_end_time - gc_start_time),
            avg_takt = mean(gc_start_time - lag_start, na.rm = TRUE))
# From this, we'd expect 50% of the measurements to have a gc_id

df1 = camml_other_wide_gcid |>
  group_by(deployment_id) |>
  summarize(pcnt_having_gcid = sum(is.finite(gc_id)) / n())
df2 = camml_gc_runs |>
  group_by(deployment_id = gc_deployment_id) |>
  summarize(med_length = median(gc_end_time - gc_start_time))
full_join(df1, df2) |>
  ggplot(aes(x = deployment_id, y = pcnt_having_gcid)) +
  geom_point(size = 3) +
  geom_point(aes(y = as.numeric(med_length)/60), color = "blue", shape = "diamond", size = 3)
# That looks about right - the gc duty cycle sets an upper limit, and sometimes we come closer to it
# and sometimes we don't.

camml_other_wide_gcid |>
  count(is.na(deployment_id))
# But every tvoc measurement has an assigned deployment id.
# That looks correct to me.

# Check that start_time_lst, deployment_id, and gc_id uniquely identify each row
camml_other_wide_gcid |>
  count(start_time_lst, deployment_id, gc_id) |>
  count(n)
# Ok, that's good.

# Add gc_id to the voc dataset:
camml_voc_gcid = left_join(camml_voc, camml_gc_runs, by = c("start_time_lst" = "gc_start_time")) |>
  select(-gc_end_time, -gc_deployment_id)

# Check the only difference is adding a gc_id column
waldo::compare(camml_voc, camml_voc_gcid)
waldo::compare(camml_other_wide, camml_other_wide_gcid)

# Write data to file
write_csv(camml_other_wide_gcid, "parsed_data/camml/camml_other_data_wide_gcid.csv")
write_csv(camml_voc_gcid, "parsed_data/camml/camml_voc_data_gcid.csv")
