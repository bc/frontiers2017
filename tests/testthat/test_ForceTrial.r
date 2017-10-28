context("Test ForceTrial Class Instantiation")

sample_posture_ForceTrials <- read_rds_to_package_extdata("force_trial_adept_x_-527.463336_adept_y_68.rds")
force_trials_list <- lapply(sample_posture_ForceTrials, ft_to_df)
full_df <- readRDS("~/Resilio Sync/data/realTimeData2017_08_16_13_23_42.rds")
str(full_df)
summary(full_df)
nrow(full_df)
print("Loading full_df. Expect 2'")

test_that("ForceTrial works with one force", {
  one_sample_settled_force <- remove_unsettled_force_trials(force_trials_list,
    0.4)[[1]]
  err = 0.4
  full_df_path <- data_location
  FT_example <- ForceTrial(one_sample_settled_force, full_df_path, full_df, err)
  attr(FT_example, "adept_coordinates") <- c(-525, 63.360715)
  expect_equal(dput(attr(FT_example, "adept_coordinates")), c(-525, 63.360715))
  expect_equal(attr(FT_example, "stability_info")[["last_n_milliseconds"]], 100)
})

test_that("we can extract the forces from the index dataframes", {
  posture_idxs_per_line <- read_rds_to_package_extdata("index_dataframes_for_two_posture_lines.rds")
  err = 0.4
  fix_x_postures <- posture_idxs_per_line[[1]]
  fix_y_postures <- posture_idxs_per_line[[2]]
  posture_tuple_list <- posture_idxs_to_index_tuples(fix_x_postures)
  ft_for_1_posture <- posture_to_ForceTrials(fix_x_postures[1:2,1], full_df, column_to_separate_forces = "reference_M0", err=0.4, last_n_milliseconds = 100)
  expect_equal(length(ft_for_1_posture[[1]][[1]]), 800)
  fts <- many_postures_to_ForceTrials(posture_tuple_list[1:2], full_df, column_to_separate_forces = "reference_M0", err=0.4, last_n_milliseconds = 100)
  expect_equal(length(fts), 2)
})


test_that("df_to_max_signed_residual gets the correct ranges", {
  one_ft <- readRDS(all_file_paths("~/Resilio Sync/data/ForceTrials_at_each_posture/")[1])[[1]]
  force_df <- ft_to_df(one_ft)
  residual <- df_to_max_signed_residual(force_df)
  expect_equal(residual, -0.144445)
  expect_equal(abs(residual), attr(one_ft,'stability_info')[['max_residual']])
})
