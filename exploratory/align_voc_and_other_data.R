library(tidyverse)
library(data.table) #We're going to be doing roll joins, which requires data.table
library(lubridate)

camml_voc = read_csv("parsed_data/camml/camml_voc_data.csv")
camml_other = read_csv("parsed_data/camml/camml_other_data.csv")

camml_voc
camml_other

camml_gc_runs = camml_voc |>
  distinct(gc_start_time = start_time_lst, gc_end_time = end_time_lst,
           gc_deployment_id = deployment_id) |>
  arrange(gc_start_time) |>
  filter(!is.na(gc_start_time), !is.na(gc_end_time)) |>
  mutate(gc_id = row_number())
plot(camml_gc_runs$gc_id, camml_gc_runs$gc_start_time)
# THat looks about what I'd expect - periods with a consistent slope with some jumps in between them
# between deployments.

camml_gc_runs_dt = data.table(camml_gc_runs)
setkey(camml_gc_runs_dt, gc_start_time)

camml_voc_dt = data.table(camml_voc)
# setkey(camml_voc_dt, voc, start_time_lst)
camml_other_dt = camml_other |>
  mutate(join_time_start = start_time_lst,
         join_time_end = end_time_lst) |>
  data.table()
# setkey(camml_other_dt, measurement, start_time_lst)
# camml_voc_dt[camml_other_dt, roll = Inf]

setkey(camml_other_dt, join_time_start)
camml_other_join1_dt = camml_gc_runs_dt[camml_other_dt, roll = Inf]
camml_other_join1_dt2 = camml_other_join1_dt %>%
  as.data.frame() %>%
  # as_tibble() %>%
  rename(gc_start_id = gc_id) %>%
  select(-gc_start_time, -gc_end_time, -gc_deployment_id) %>%
  data.table()
setkey(camml_other_join1_dt2, join_time_end)
setkey(camml_gc_runs_dt, gc_end_time)
camml_other_join2_dt = camml_gc_runs_dt[camml_other_join1_dt2, roll = -Inf]
# q = camml_other_join2_dt %>%
#   as_tibble() %>%
#   select(-gc_start_time, -gc_end_time)
# q |>
#   filter(gc_start_id == gc_id) |>
#   filter(deployment_id != gc_deployment_id,
#          measurement == "RH") |>
#   count(deployment_id, deployment_name)
#
#   count(gc_id == gc_start_id,
#         deployment_id == gc_deployment_id)
camml_other_aligned = camml_other_join2_dt %>%
  as_tibble() %>%
  select(-gc_start_time, -gc_end_time) %>%
  filter(deployment_id == gc_deployment_id) %>%
  mutate(gc_id = ifelse(gc_start_id == gc_id, gc_id, NA)) %>%
  select(-gc_start_id, -gc_deployment_id) %>%
  left_join(camml_gc_runs, by = c("gc_id"))
camml_other_aligned
# Check that ... gc_id, start_time_lst, and measurement uniquely identifies each row
camml_other_aligned |>
  filter(measurement == "RH") |>
  group_by(gc_id, start_time_lst, measurement) |>
  filter(n() > 1)
# Ok! That looks good now

camml_other_gc = camml_other_aligned %>%
  filter(!is.na(gc_id)) %>%
  group_by(gc_id, measurement, units, gc_start_time, gc_end_time) %>%
  summarize(value = mean(value, na.rm = TRUE),
            .groups = "drop")


camml_benzene = camml_voc %>%
  filter(voc == "benzene") %>%
  select(start_time_lst, voc, flags, benzene = concentration, deployment_id) %>%
  left_join(camml_gc_runs_dt, by = c(start_time_lst = "gc_start_time"))

camml_pid = camml_other_gc %>%
  filter(measurement == "VOC(pid)") %>%
  transmute(gc_id,  tvoc = value)

combined_df = full_join(camml_benzene, camml_pid, by = "gc_id")
ggplot(combined_df, aes(x = benzene, y= tvoc)) +
  geom_hex()

lm(benzene ~ tvoc, data = combined_df) %>%
  summary()

# So there is a statistically significant relationship between the 2, but the R2 is 0.05
# So there's basically no predictive power from knowing the TVOC.
combined_df

lm(benzene ~ tvoc + factor(deployment_id) + factor(hour(start_time_lst)), data = combined_df) %>%
  arm::display(digits = 4)

# Other information I'd want to think about?
# 1. Local conditions? Like what direction are the nearby oil and gas wells? And in what direction
# is the wind coming from?
# 2. Similarly, plumes? Is there a reasonable way to distingush plumes from other events? Is that even relevant though, since we only have hour long GC measurements? Probably! Like, knowing the maximum TVOC might tell me something too!
# I guess I might want to standardize it somehow? I'm not sure. But I can totally imagine that a background and a plume might have different sorts of relationships.

# Ok, I'm liking this idea of trying to do some machine learning-type analyses on the data. But if
# I'm going to do that, I'm going to need to need to do it properly - split into test and training,
# build my models, and then evaluate.
# And I'd need to get back up to speed

# Ok, writing this data to file and then moving to a new script
camml_voc_aligned = camml_voc |>
  left_join(camml_gc_runs, by = c(start_time_lst = "gc_start_time"))
write_csv(camml_other_aligned, "parsed_data/camml/camml_other_data_gcid.csv")
write_csv(camml_voc_aligned, "parsed_data/camml/camml_voc_data_gcid.csv")
