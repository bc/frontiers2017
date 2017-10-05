context("Munging realtimedata files")
require(testthat)
source("../../R/settling_time_analysis.r")
source("../../R/functions_specific_to_frontiers2017_dataset.r")
source("../../R/generic_tools.r")
source("../../R/time_series_functions.r")

test_that("one can remove nonstabilized force trials for few postures", {
  posture_samples_n_100_fix_x <- rds_from_package_extdata("posture_samples_n_100_fix_x.rds")
  test_example_force_trial <- posture_samples_n_100_fix_x[[1]][[1]]
  first_posture_stablizes <- force_trial_does_stabilize(test_example_force_trial,
    muscle = "M0", err = 0.5)
  too_stringent_yields_false <- force_trial_does_stabilize(test_example_force_trial,
    muscle = "M0", err = 0.005)
  expect_true(first_posture_stablizes)
  expect_false(too_stringent_yields_false)
})

test_that("one can remove nonstabilized force trials for 100 postures in y", {
  posture_samples_n_100_fix_x <- rds_from_package_extdata("posture_samples_n_100_fix_x.rds")
  all_force_trials <- unlist(posture_samples_n_100_fix_x, recursive = FALSE)
  all_settled_force_trials <- remove_unsettled_force_trials(all_force_trials, 0.4)
  different_errors <- seq(0.1, 1, length.out = 20)
  require(parallel)
  num_remaining_force_trials <- unlist(mclapply(different_errors, function(err) {
    length(remove_unsettled_force_trials(all_force_trials, err))
  }))
  pdf("../../../output/force_trial_yield_under_settling_time_error_threshold.pdf",
    width = 10, height = 10)
    plot_remaining_force_trial_fraction_as_function_of_err(different_errors, num_remaining_force_trials, length(all_force_trials))
  dev.off()
  force_trials_that_settled <- remove_unsettled_force_trials(all_force_trials,stabilization_err_99_percentile)
  percentage_of_trials_remaining_with_err_threshold <- length(force_trials_that_settled)/10000
  expect_true(abs(0.99 - percentage_of_trials_remaining_with_err_threshold) < 0.01)
  stabilization_statistic <- paste0(percentage_of_trials_remaining_with_err_threshold,"% of trials remain with a stabilization metric of ", stabilization_err_99_percentile, "N")
  print(stabilization_statistic)
})
