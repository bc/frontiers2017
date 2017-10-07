context("Munging realtimedata files")
require(testthat)
source("../../R/settling_time_analysis.r")
source("../../R/functions_specific_to_frontiers2017_dataset.r")
source("../../R/generic_tools.r")
source("../../R/time_series_functions.r")
source("../../R/force_trial_stability.r")


test_that("ForceTrial works with one force", {
  all_force_trials_fix_x <- unlist(read_rds_to_package_extdata("posture_samples_n_100_fix_x.rds"), recursive = FALSE)[1]
  one_sample_settled_force <- remove_unsettled_force_trials(all_force_trials_fix_x, 0.4)[[1]]
  err = 0.4
  full_df_path <- data_location
  full_df <- readRDS("~/Resilio Sync/data/realTimeData2017_08_16_13_23_42.rds"); print("Loading full_df. Expect 2'")
  FT_example <- ForceTrial(one_sample_settled_force, full_df_path, full_df, err)
  expect_equal(dput(attr(FT_example,"adept_coordinates")), c(-525, 63.360715))
  expect_equal(attr(FT_example,"stability_info")[['last_n_milliseconds']], 100)
  expect_equal(attr(FT_example,"stability_df")[['initial_reference_force']], 18.225344)
})


test_that('we can extract the forces from the index dataframes', {
  posture_idxs_per_line <- read_rds_to_package_extdata('index_dataframes_for_two_posture_lines.rds')
  full_df <- readRDS("~/Resilio Sync/data/realTimeData2017_08_16_13_23_42.rds"); print("Loading full_df. Expect 2'")
  fix_x_postures <- posture_idxs_per_line[[1]]
  fix_y_postures <- posture_idxs_per_line[[2]]
  forceTrials_fix_x <- unlist(posture_start_finish_indices_to_L_of_ForceTrials(fix_x_postures, full_df, data_location, err), recursive=FALSE)
  browser()
  remove_unsettled_force_trials(all_force_trials_fix_x, 0.4)
})
