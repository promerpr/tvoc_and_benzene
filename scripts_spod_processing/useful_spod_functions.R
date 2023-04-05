fix_spod_colnames = function(raw_df) {
  raw_df[map_lgl(raw_df, \(x) identical(x, rep(TRUE, length(x))))] = "T"

  wanted_cols = c("DATE", "PID1", "T", "RH", "P", "WS", "WD", "TC", "BATT", "CHRG",
                  "RUN", "TRIG", "LAT", "LON", "CRC")
  inds = map_dbl(wanted_cols, \(x) match(x, raw_df[1, ]))
  ind_df = data.frame(names = wanted_cols,
                      inds = inds) |>
    arrange(inds) |>
    filter(!is.na(inds))
  new_names = colnames(raw_df)
  for (i in seq_len(nrow(ind_df))) {
    ii = ind_df$inds[[i]]
    next_ii = ifelse(i < nrow(ind_df), ind_df$ind[[i+1]], ncol(raw_df)+1) - 1
    n = next_ii - ii
    if (n > 1) {
      new_names[(ii+1):next_ii] = paste0(ind_df$names[[i]], "_", seq_len(n))
    } else {
      new_names[(ii+1):next_ii] = ind_df$names[[i]]
    }
  }
  df = raw_df
  colnames(df) = new_names
  df = df[, -ind_df$inds]
  if ("CRC" %in% colnames(df)) {
    df = select(df, -CRC)
  }
  df = df |>
    rename(SPOD_NAME = X1)
  return(df)
}

clean_up_raw_spod = function(df) {
  if (!"LON" %in% colnames(df)) {
    df  = df |>
      rename(LON = LON_1)
  }
  df |>
    mutate(LON = as.numeric(str_remove_all(LON, "[zZ]$")),
           DATE = mdy_hms(DATE),
           SPOD_ID = str_remove_all(SPOD_NAME, "^SPOD0")) |>
    relocate(SPOD_ID, .after = SPOD_NAME)
}

munique = function(vec, na.rm = TRUE) {
  if (is.numeric(vec) | is.timepoint(vec)) {
    mean(vec, na.rm = na.rm)
  } else {
    unique(vec)
  }
}
downsample_spod = function(df, period = "5 minutes") {
  df |>
    mutate(.date_id = floor_date(DATE, unit = period)) |>
    group_by(.date_id) |>
    reframe(across(everything(), munique, na.rm = TRUE)) |>
    select(-.date_id)

}
