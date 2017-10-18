context("Munging realtimedata files")
require(testthat)
source("../../R/settling_time_analysis.r")
source("../../R/functions_specific_to_frontiers2017_dataset.r")
source("../../R/generic_tools.r")
source("../../R/time_series_functions.r")
source("../../R/force_trial_stability.r")

pbmclapply <- pblapply
mclapply <- pblapply

sample_posture_ForceTrials <- read_rds_to_package_extdata("force_trial_adept_x_-527.463336_adept_y_68.rds")
force_trials_list <- lapply(sample_posture_ForceTrials, ft_to_df)


test_that("one can remove nonstabilized force trials for few postures", {
  test_example_ForceTrial <- ft_to_df(sample_posture_ForceTrials[[1]])
  first_posture_stablizes <- force_trial_does_stabilize(test_example_ForceTrial,
    muscle = "M0", err = 0.5)
  too_stringent_yields_false <- force_trial_does_stabilize(test_example_ForceTrial,
    muscle = "M0", err = 0.005)
  expect_true(first_posture_stablizes)
  expect_false(too_stringent_yields_false)
})

test_that("we can create a conglomerate stability_df for 100 forces in a Posture",
  {
    stability_df <- ForceTrials_to_stability_df(sample_posture_ForceTrials)
    pdf("../../../output/tension_settling_scatter_for_sample_posture_n=100_forcetrials.pdf",
      width = 10, height = 10)
    tension_settling_scatter(stability_df)
    dev.off()
  })

test_that("one can remove nonstabilized force trials for 100 postures in y", {
  ######
  pdf("../../../output/force_trial_yield_under_settling_time_error_threshold.pdf",
    width = 10, height = 10)
    try_different_stability_thresholds(force_trials_list, 200)
  dev.off()
  ######
})

test_that("we can evaluate the percentage of forcetrials that settled under an arbitrary threshold", {
  force_trials_that_settled_fix_x <- remove_unsettled_force_trials(force_trials_list,
    stabilization_err_99_percentile)
  percentage_of_trials_remaining_with_err_threshold <- length(force_trials_that_settled_fix_x)/100
  expect_true(abs(0.98 - percentage_of_trials_remaining_with_err_threshold) < 0.01)
  stabilization_statistic <- paste0(percentage_of_trials_remaining_with_err_threshold,
    "% of trials remain with a stabilization metric of ", stabilization_err_99_percentile,
    "N")
})

test_that('we can remove unsettled force trials', {
  print("Removing unsettled force trials")
  settled_forcetrials <- remove_unsettled_force_trials(force_trials_list, 0.4)
    expect_true(length(force_trials_list) >= length(settled_forcetrials))
  })

test_that('we can plot stability_metrics for 1 posture', {
  #TODO implement with all postures
    stability_metrics <- ForceTrials_to_stability_info_df(sample_posture_ForceTrials)
  ######
  pdf("../../../output/sample_posture_settling_time_analysis.pdf", width = 10, height = 10)
  plot(stability_metrics$reference, stability_metrics$max_residual, col = scales::alpha("black",
    0.15), pch = 20, xlab = "Reference force for M0", main = "Sample of 100 postures (fixed-x), n=100 forces per posture.",
    ylab = "Max Residual from reference in last 100ms")
  hist(stability_metrics[, 3], breaks = 10, col = "black", xlim = c(0, max(stability_metrics[, 3])), xlab = "Maximum residual from desired force in last 100ms",
    main = "Sample of 1 postures (fixed-x), n=100 forces at this posture.")
    dev.off()
  })

test_that('we can plot stability_df for 1 posture', {
  stability_df <- ForceTrials_to_stability_df(sample_posture_ForceTrials)
  reasonable_delta_force <- abs(stability_df$delta_force) > 1
  stability_df_no_small_deltas <- stability_df[reasonable_delta_force, ]
  pdf("../../../output/sample_posture_stability_df.pdf", width = 10, height = 10)
  hist(stability_df$amortized_velocity_of_force * 1000, breaks = 20, cex = 0.15, col='black',
    pch = 19, xlab = "d(tension)/dt  (Newtons/s)", main = "Amortized rate of change in M0 tension across all force trials")
  settling_time_histogram_for_posture(stability_df, breaks = 20)
  print(summary(stability_df))
  dev.off()
})

test_that('we can plot stability_df for all postures in X', {
  rds_postures <- all_file_paths("~/Resilio Sync/data/ForceTrials_at_each_posture/")
  stability_df <- do.call('rbind', pblapply(rds_postures, function(rds_path){
    ForceTrials_to_stability_df(readRDS(rds_path))
  }))s
  reasonable_delta_force <- abs(stability_df$delta_force) > 1
  stability_df_no_small_deltas <- stability_df[reasonable_delta_force, ]
  pdf("../../../output/posture_stability_df.pdf", width = 10, height = 10)
  hist(stability_df$amortized_velocity_of_force * 1000, breaks = 200, cex = 0.15, col='black',
    pch = 19, xlab = "d(tension)/dt  (Newtons/s)", main = "Amortized rate of change in M0 tension across all force trials?")
  settling_time_histogram_for_posture(stability_df, breaks = 200)
  tension_settling_scatter(stability_df)
  abs_value_delta_force_scatter(stability_df)
  print(summary(stability_df))
  dev.off()

})
