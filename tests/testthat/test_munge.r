context("Munging realtimedata files")
require(testthat)
source("../../R/settling_time_analysis.r")
source("../../R/functions_specific_to_frontiers2017_dataset.r")
source("../../R/generic_tools.r")
source("../../R/time_series_functions.r")
source("../../R/force_trial_stability.r")

sample_posture_ForceTrials <- read_rds_from_package_extdata("force_trial_adept_x_-527.463336_adept_y_68.rds")
force_trials_list <- lapply(sample_posture_ForceTrials, ft_to_df)

pbmclapply <- pblapply
pbmcapply <- pbapply
mclapply <- pblapply

context("delta force")
test_that("delta force visualizations work", {
  stability <- read_rds_from_package_extdata("stability_dataframes_for_both_lines.rds")
  stability_a <- stability[stability$settling_time < 700, ]
  print(summary(stability))
  reasonable_delta_force <- abs(stability$delta_force) > 1
  stability_df_no_small_deltas <- stability[reasonable_delta_force, ]
  pdf("../../output/stability_df_stats.pdf", width = 10, height = 10)

  hist(stability[['max_residual']], breaks = 500, col = "black", xlab = "Highest magnitude of residual observed in last 100ms of force trial (N)",
    ylab = "Number of force trials", main = paste("n =", length(c), "force trials"))


settling_times <- stability[['settling_time']]
  plot(ecdf(stability$settling_time), xlab = "Settling time (ms)", ylab = "Fraction of samples",
    main = "Empirical cumulative distribution function")
    #fn via https://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset
    remove_outliers <- function(x, na.rm = TRUE, ...) {
      qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
      H <- 1.5 * IQR(x, na.rm = na.rm)
      y <- x
      y[x < (qnt[1] - H)] <- NA
      y[x > (qnt[2] + H)] <- NA
      y
    }

  velocities  <- na.omit(remove_outliers(stability$amortized_velocity_of_force))
  num_outlier_forcetrials_removed <- length(stability$amortized_velocity_of_force) - length(velocities)
  hist(velocities * 1000, breaks = 200, cex = 0.08,
    col = "black", pch = 19, xlab = "d(tension)/dt  (Newtons/s)", main = paste("Amortized M0 tension rate across ",length(velocities)," force trials", num_outlier_forcetrials_removed, "outliers were removed (outlier = beyond 1.5+-IQR."))
  settling_time_histogram_for_posture(stability, breaks = 200)
  dev.off()
  p1 <- tension_settling_scatter(stability)
  ggsave("../../output/stability_df_deltaforce.pdf", p1, width = 7, height = 6,
    units = "in")

#TODO consider rm'ing samples with post-700 settling time
  deltaforce_settling_time <- abs_value_delta_force_scatter(stability, pointsize = 0.05)
  ggsave("../../output/stability_df_deltaforce_abs.pdf", deltaforce_settling_time,
    width = 7, height = 6, units = "in")
})

context("Manipulations based on stabilization")

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
    tension_settling_scatter(stability_df)
  })

test_that("one can remove nonstabilized force trials for 100 postures in y", {
  ######
  pdf("../../output/force_trial_yield_under_settling_time_error_threshold.pdf",
    width = 10, height = 10)
  try_different_stability_thresholds(force_trials_list, 200, muscles_of_interest = muscle_names())
  dev.off()
  ######
})

context("Evaluating remaining solutions when threshold restrictions are applied to stability")
test_that("we can evaluate the percentage of forcetrials that settled under an arbitrary threshold",
  {
    force_trials_that_settled_fix_x <- remove_unsettled_force_trials(force_trials_list,
      stabilization_err_99_percentile)
    percentage_of_trials_remaining_with_err_threshold <- length(force_trials_that_settled_fix_x)/100
    expect_true(abs(0.98 - percentage_of_trials_remaining_with_err_threshold) <
      0.01)
    stabilization_statistic <- paste0(percentage_of_trials_remaining_with_err_threshold,
      "% of trials remain with a stabilization metric of ", stabilization_err_99_percentile,
      "N")
  })

test_that("we can remove unsettled force trials", {
  print("Removing unsettled force trials")
  settled_forcetrials <- remove_unsettled_force_trials(force_trials_list, 0.4)
  expect_true(length(force_trials_list) >= length(settled_forcetrials))
})

test_that("we can plot stability_metrics for 1 posture", {
  # TODO implement with all postures
  stability_metrics <- ForceTrials_to_stability_info_df(sample_posture_ForceTrials)
  ######
  pdf("../../output/sample_posture_settling_time_analysis.pdf", width = 10,
    height = 10)
  plot(stability_metrics$reference, stability_metrics$max_residual, col = scales::alpha("black",
    0.15), pch = 20, xlab = "Reference force for M0", main = "Sample of 100 postures (fixed-x), n=100 forces per posture.",
    ylab = "Max Residual from reference in last 100ms")
  hist(stability_metrics[, 3], breaks = 10, col = "black", xlim = c(0, max(stability_metrics[,
    3])), xlab = "Maximum residual from desired force in last 100ms", main = "Sample of 1 postures (fixed-x), n=100 forces at this posture.")
  dev.off()
})

test_that("we can plot stability_df for 1 posture", {
  stability_df <- ForceTrials_to_stability_df(sample_posture_ForceTrials)
  reasonable_delta_force <- abs(stability_df$delta_force) > 1
  stability_df_no_small_deltas <- stability_df[reasonable_delta_force, ]
  pdf("../../output/sample_posture_stability_df.pdf", width = 10, height = 10)
  hist(stability_df$amortized_velocity_of_force * 1000, breaks = 20, cex = 0.15,
    col = "black", pch = 19, xlab = "d(tension)/dt  (Newtons/s)", main = "Amortized rate of change in M0 tension across all force trials")
  settling_time_histogram_for_posture(stability_df, breaks = 20)
  print(summary(stability_df))
  dev.off()
})
