df = camml_other_wide_pad |>
  rename(tvoc = `VOC(pid)`) %>%
  .[1:100, ]
df

f1 = function(df) {
  mutate(df,
         tvoc_min_7day = slide_dbl(tvoc, min, .before = 24*60*7, .complete = FALSE, na.rm = TRUE))
}

f2 = function(df) {
  df$tvoc_min_7day = NA
  for (i in seq_len(nrow(df))) {
    i_start = max(i - 24*60*7, 1)
    df$tvoc_min_7day[[i]] = min(df$tvoc[i_start:i], na.rm = TRUE)
  }
  df
}

f2(df)
f1(df)
waldo::compare(f1(df), f2(df))
comp = bench::mark(f1(df), f2(df))
comp$memory

df2 = data.frame(tvoc = 1:100)
comp = bench::mark(f1(df2), f2(df2))
comp

# Ok, the issue seems to be rlang-style warning messages. That's really annoying.

# Can I solve it with suppress warnings?
swmin = function(..., na.rm = TRUE) {
  suppressWarnings(min(..., na.rm = na.rm))
}

f1sw = function(df) {
  mutate(df,
         tvoc_min_7day = slide_dbl(tvoc, swmin, .before = 24*60*7, .complete = FALSE))
}

f2sw = function(df) {
  df$tvoc_min_7day = NA
  for (i in seq_len(nrow(df))) {
    i_start = max(i - 24*60*7, 1)
    df$tvoc_min_7day[[i]] = swmin(df$tvoc[i_start:i])
  }
  df
}

df_e4 = camml_other_wide_pad |>
  rename(tvoc = `VOC(pid)`) %>%
  .[1:10000, ]
bench::mark(f1sw(df_e4), f2sw(df_e4))
# Ok, in addition to the warnings issue, there is also something else going on, where
# slide grows non-linearly with the length of the dataset.

time_df = tribble(~amt, ~time,
        1e5, 26,
        1e4, 1.57,
        1e3, 0.1,
        1e2, 898/(1e6))
ggplot(time_df, aes(x = amt, y = time)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

# Hrm. Well, that's a project for another day.


sw_slide = function(df, n) {
  df = df[1:n, ]
  mutate(df,
         tvoc_min_7day = slide_dbl(tvoc, swmin, .before = 24*60*7, .complete = FALSE))
}

sw_loop = function(df, n) {
  df = df[1:n, ]
  df$tvoc_min_7day = NA
  for (i in seq_len(nrow(df))) {
    i_start = max(i - 24*60*7, 1)
    df$tvoc_min_7day[[i]] = swmin(df$tvoc[i_start:i])
  }
  df
}

namin = function(..., na.rm = TRUE) {
  min(..., na.rm = na.rm)
}

f_either = function(df, n, suppress_warnings, method = c("slide", "loop")) {
  method = match.arg(method)
  df = df[1:n, ]
  if (suppress_warnings) {
    f = swmin
  } else {
    f = namin
  }
  if (method == "slide") {
    mutate(df,
           tvoc_min_7day = slide_dbl(tvoc, f, .before = 24*60*7, .complete = FALSE))
  } else {
    df$tvoc_min_7day = NA
    for (i in seq_len(nrow(df))) {
      i_start = max(i - 24*60*7, 1)
      df$tvoc_min_7day[[i]] = f(df$tvoc[i_start:i])
    }
    df
  }
}

timing_res =
  bench::press(n = 10^c(1, 2, 3, 4),
             suppress_warnings = c(TRUE, FALSE),
             method = c("slide", "loop"),
             {
               bench::mark(f_either(df, n, suppress_warnings, method))
             })
ggplot(timing_res, aes(x = n, y = as.numeric(median), color = method, shape = suppress_warnings,
                       linetype = suppress_warnings)) +
  geom_point() +
  geom_line() +
  scale_x_log10() +
  scale_y_log10()

f_full = function() {
  camml_other_wide_pad |>
    rename(tvoc = `VOC(pid)`) |>
    mutate(tvoc_min_7day = slide_dbl(tvoc, swmin, .before = 24*60*7, .complete = FALSE, na.rm = TRUE))
}
bench::mark(f_full())
