context("Test tendons fixed to a nonmoving post")
df <- as.data.frame(fread("/Users/briancohn/Resilio Sync/data/realTimeData2017_09_24_12_25_56.txt"))
colnames(df) <- hyphens_to_dots(colnames(df))
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
  fts <- posture_to_ForceTrials(c(784032, 800032), df, column_to_separate_forces,
    err, last_n_milliseconds)
  grobs <- lapply(fts, post_tensions_forces_over_time)
  stabilized_muscles <- lapply(fts, function(x) {
    which_muscles_stabilized(ft_to_df(x), err = 0.4)
  })
  g <- gridExtra::arrangeGrob(grobs = grobs, nrow = 6)
  ggsave(g, dpi = 100, device = "png", filename = "q.png")
  first_four_muscles <- muscle_names()[1:4]
  s<- lapply(fts, function(ft){
    stability_per_muscle(ft_to_df(ft), muscles_of_interest = first_four_muscles, last_n_milliseconds=100)
  })
})

stability_per_muscle <- function(ft_df, muscles_of_interest,...){
  by_muscle_stable_metrics <- lapply(muscles_of_interest, function(x) force_trial_to_stable_metrics(ft_df, 100,x))
  by_muscle_stable_metrics
  by_muscle_stable_dfs <- lapply(muscles_of_interest, function(x) list_of_forces_to_stabilized_df(list(ft_df), err, df, x))
  print('finstabledf')
  browser()
}

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
