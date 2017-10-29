context("Test tendons fixed to a nonmoving post")
df <- as.data.frame(fread("/Users/briancohn/Resilio Sync/data/realTimeData2017_09_24_12_25_56.txt"))

test_that("we can extract the forces", {
  message("Identifying unique postures. Expect competion in ")
  unique_postures <- tail(unique(df[c("adept_x", "adept_y")]), 2)
  message("Grouping postures by line")
  postures_per_line <- postures_grouped_by_line(unique_postures, x_fixed_val = -525,
    y_fixed_val = 68)
  message("Identifying indices for the start and end of each posture")
  idx_dfs_df <- postures_to_idx_dfs_for_post(rev(postures_per_line), unique_postures)
  idx_dfs <- df_to_list_of_rows(idx_dfs_df)
  fix_y_postures <- idx_dfs[[1]]
  fix_x_postures <- idx_dfs[[2]]
  column_to_separate_forces <- reference("M0")
  err <- 0.4
  last_n_milliseconds <- 100
  # to save to RDS: saveRDS(idx_dfs, 'index_dataframes_for_two_posture_lines.rds')
  fts <- many_postures_to_ForceTrials(posture_idxs_to_index_tuples(fix_x_postures), df, column_to_separate_forces, err=0.4, last_n_milliseconds, save_rds=TRUE, prefix="post_experiment")


  fts <- posture_to_ForceTrials(as.numeric(idx_dfs[1, 1:2]), df, column_to_separate_forces,
    err, last_n_milliseconds)
  message("Splitting by force trials")
  list_of_postures <- split(df, list(df$adept_x, df$adept_y), drop = TRUE)
  saveRDS(fts, "postures_for_static_post_experiment.rds")
})

test_that("actions have no effect upon JR3 Force", {
  names(df) <- hyphens_to_underscores(names(df))
  downsampled_df <- df[seq(1, nrow(df), by = 100)]
  p <- ggplot(downsampled_df, aes(x = time))
  p <- p + geom_line(aes(y = reference_M0/max(reference_M0), color = measured_M0),
    size = 0.1)
  p <- p + geom_line(aes(y = adept_x), size = 0.1)
  p <- p + geom_line(aes(y = adept_y), size = 0.1)
  p <- p + geom_point(aes(y = JR3_FY/max(JR3_FY)), size = 0.1)
  p
})
