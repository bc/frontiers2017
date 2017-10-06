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



# test_that('we can extract the forces from the index dataframes', {
# idx_dfs <- rds_from_package_extdata("index_dataframes_for_two_posture_lines.rds")
#
# })


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

  full_df <- readRDS("~/Resilio Sync/data/realTimeData2017_08_16_13_23_42.rds")
  stability_df <- list_of_forces_to_stabilized_df(all_settled_force_trials, data_location, full_df=full_df, muscle_of_interest = "M0", err=0.4)
  stability_metrics_df <- posture_list_to_stability_metrics_df_rows(all_settled_force_trials, last_n_milliseconds = 100, muscle = "M0")

  pdf("../../../output/settling_time_analysis.pdf", width = 10, height = 10)
    plot(stability_metrics_df$reference, stability_metrics_df$max_residual, col = scales::alpha('black', 0.15), pch=20, xlab="Reference force for M0", main = "Sample of 100 postures (fixed-x), n=100 forces per posture.", ylab="Max Residual from reference in last 100ms")
    tension_settling_scatter(stability_df)
    hist(stability_metrics_df[,3], breaks=50, col='black', xlim=c(0, 2), xlab="Maximum residual from desired force in last 100ms", main = "Sample of 100 postures (fixed-x), n=100 forces per posture.")
    plot(stability_df$delta_force, stability_df$amortized_velocity_of_force)
    settling_time_histogram_for_posture(stability_df, breaks=20)
  dev.off()
  browser()
  list_of_posture_and_stability <- lapply(df_to_list_of_rows(stability_df), function(force_trial_stability_info) {
      initial_observation <- full_df[force_trial_stability_info$initial_index,]
      info_with_posture <- cbind(force_trial_stability_info,data.frame(adept_x=initial_observation$adept_x, adept_y=initial_observation$adept_y))
      return(info_with_posture)
  })
})
