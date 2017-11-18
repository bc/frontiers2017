context("Testing full-scale full-data tests. Set a timer because it'll be time intensive!")

sample_posture_ForceTrials <- read_rds_from_package_extdata("force_trial_adept_x_-527.463336_adept_y_68.rds")
force_trials_list <- lapply(sample_posture_ForceTrials, ft_to_df)
print("Loading full_df. Expect 2'")
full_df <- readRDS("~/Resilio Sync/data/realTimeData2017_08_16_13_23_42.rds")
str(full_df)
summary(full_df)
nrow(full_df)

context("ForceTrial and 1 force input")
test_that("ForceTrial works with one force", {
  one_sample_settled_force <- remove_unsettled_force_trials(force_trials_list,
    0.4)[[1]]
  err = 0.4
  full_df_path <- data_location
  FT_example <- ForceTrial(one_sample_settled_force, full_df, err)
  attr(FT_example, "adept_coordinates") <- c(-525, 63.360715)
  expect_equal(dput(attr(FT_example, "adept_coordinates")), c(-525, 63.360715))
  expect_equal(attr(FT_example, "stability_info")[["last_n_milliseconds"]], 100)
})

context('multi-input to forcetrial')
test_that("we can extract the forces from the index dataframes", {
  posture_idxs_per_line <- read_rds_from_package_extdata("index_dataframes_for_two_posture_lines.rds")
  err = 0.4
  fix_x_postures <- posture_idxs_per_line[[1]]
  fix_y_postures <- posture_idxs_per_line[[2]]
  posture_tuple_list <- posture_idxs_to_index_tuples(fix_x_postures)
  ft_for_1_posture <- posture_to_ForceTrials(fix_x_postures[1:2,1], full_df, column_to_separate_forces = "reference_M0", err=0.4, last_n_milliseconds = 100)
  expect_true(abs(length(ft_for_1_posture[[1]][[1]])-800) < 10)
  fts <- many_postures_to_ForceTrials(posture_tuple_list[1:2], full_df, column_to_separate_forces = "reference_M0", err=0.4, last_n_milliseconds = 100)
  expect_equal(length(fts), 2)
})


context('stability combo plot for both lines')
rds_postures <- all_file_paths("~/Resilio Sync/data/ForceTrials_at_each_posture/")
test_that("we can plot stability_df for all postures in X", {
  #Note these ones include all Force trials, even ones that did not "settle"
  print('expect 6 minutes to load & compute all postures into the stability df')
  stability_df_for_both_posture_lines <- get_stability_df_for_all_postures(rds_postures)
  x_and_y <- split_stability_df_into_postures(stability_df_for_both_posture_lines)
  fix_x <- x_and_y$fix_x
  fix_y <- x_and_y$fix_y
  along_x_stability <- produce_stability_plots(fix_y, adept_dimension_that_changes='adept_x')
  along_y_stability <- produce_stability_plots(fix_x, adept_dimension_that_changes='adept_y')
  g <- arrangeGrob(grobs = c(along_x_stability, along_y_stability), nrow = 2)
  ggsave("../../output/static_motor_control_properties.pdf", g, device = "pdf", width = 8, height = 4,
  scale = 4, limitsize = FALSE, dpi = 100)
})
