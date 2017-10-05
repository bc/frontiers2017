
require(testthat)
source("../../R/settling_time_analysis.r")
source("../../R/force_trial_stability.r")
source("../../R/generic_tools.r")
source("sample_datasets.r")
sample_vec <- c(1, 1, 1, 1, 1, 1, 4, 8, 9, 4, 5, 3, 2, 3, 2, 3, 2, 3, 3, 3, 3, 3,
  3, 3)

context("Evaluators of stability")
test_that("stabilized", {
  expect_that(stabilized(sample_vec, 3, 1), is_false())
  expect_that(stabilized(sample_vec[12:length(sample_vec)], 3, 1), is_true())
  expect_that(stabilized(sample_vec[7], 3, 1), is_true())
  expect_that(stabilized(c(2.5), 2, 1), is_true())
  expect_that(stabilized(c(1.00001), 0, 1), is_false())
  expect_that(stabilized(c(-1), 1, 1), is_false())
  expect_that(stabilized(c(-1), 1, -1), throws_error())
})

context("Bisection method for identifying stability index")

test_that("integer_midpoint", {
  expect_equal(integer_midpoint(c(-10, 100)), 45)
  expect_equal(integer_midpoint(c(-10, 10)), 0)
  expect_equal(integer_midpoint(c(0, 10)), 5)
  expect_equal(integer_midpoint(c(-10.241, 1.7521)), -5)
  expect_equal(integer_midpoint(c(-12.241, 1.7521)), -6)
})

test_that("bound_width", {
  expect_equal(bound_width(c(1, 5)), 4)
})

test_that("first_true_value_idx", {
  expect_equal(first_true_value_idx(FALSE, TRUE, c(6, 7)), 7)
  expect_equal(first_true_value_idx(TRUE, TRUE, c(6, 7)), 6)
  expect_that(first_true_value_idx(FALSE, FALSE, c(6, 7)), throws_error())
})

test_that("stabilized_index", {
  expect_equal(stabilized_index(c(1, 1, 5, 1, 1, 1, 1, 3, 3), 3, 1), 8)
  expect_equal(stabilized_index(c(1, 1, 5, 1, 1, 1, 1, -3, -3), -3, 1), 8)
  expect_equal(stabilized_index(c(-3), -3, 1), 1)
  expect_equal(stabilized_index(c(-2.888), -3, 1), 1)
  expect_that(stabilized_index(c(1, 1), -3, 1), throws_error())
  expect_that(stabilized_index(c(-1, -1), -3, 1), throws_error())
  expect_equal(stabilized_index(sample_vec, 3, 1), 12)
  expect_equal(stabilized_index(sample_vec, 3, 1), 12)
  expect_equal(stabilized_index(sample_measured_M0_force_trial, 4, 0.5), 207)
})

test_that("performance of stabilized_index is acceptable", {
  library(microbenchmark)
  replicates = 1000
  res = microbenchmark(slow_stabilized_index(sample_vec, desired = 3, err = 1),
    slow_stabilized_index(sample_measured_M0_force_trial, desired = 4, err = 0.5),
    stabilized_index(sample_vec, desired = 3, err = 1), stabilized_index(sample_measured_M0_force_trial,
      desired = 4, err = 0.5), times = replicates)
  pdf("../../../output/settling_time_analysis_performance.pdf", width = 10, height = 10)
  plot(res)
  dev.off()
  expect_equal(1, 1)  #this is here invoke the block & ensure no errors happen in the above code.
})

test_that("discrete_diff", {
  expect_equal(discrete_diff(c(1, 2, 3)), c(1, 1))
  expect_equal(discrete_diff(c(10, 10, 10, 10)), c(0, 0, 0))
  expect_equal(discrete_diff(c(10, -10, 10, -10)), c(-20, 20, -20))
})

test_that("can_eval_stabilize_idx_to_2_postures", {
  # load a list of postures fixed in X, each a list of force trials DFs.
  print("Loading full_df. Expect 2'")
  full_df <- readRDS("~/Resilio Sync/data/realTimeData2017_08_16_13_23_42.rds")
  print("Loading precomputed posture samples")
  posture_samples_n_100_fix_x <- rds_from_package_extdata("posture_samples_n_100_fix_x.rds")

  v <- list_of_postures_of_forces_to_stabilized_df(posture_samples_n_100_fix_x[1:100], full_df_path = data_location,
    err = 0.5, full_df, muscle_of_interest = "M0")
  # get_reference_to_variance_relationship_df(x, 30, 'M0')}) a <- do.call('rbind',)
  # plot(tsd, xlab='M0 reference force (N)', ylab = 'SD(last 100 ms of force
  # trial)', pch=19)

  stability_df <- do.call("rbind", list_of_postures_of_forces_to_stabilized_df(posture_samples_n_100_fix_x[1:2][-13][-14],
    full_df_path = data_location, err = 0.5, full_df, muscle_of_interest = "M0"))

  stability_metrics_df <- do.call("rbind", lapply(posture_samples_n_100_fix_x[1:100], posture_list_to_stability_metrics_df_rows, last_n_milliseconds = 100, muscle = "M0"))
  pdf("../../../output/settling_time_analysis.pdf", width = 10, height = 10)
  hist(stability_metrics_df[,3], breaks=50, col='black', xlim=c(0, 2), xlab="Maximum residual from desired force in last 100ms", main = "Sample of 100 postures (fixed-x), n=100 forces per posture.")
  plot(stability_metrics_df$reference, stability_metrics_df$max_residual, col = scales::alpha('black', 0.15), pch=20, xlab="Reference force for M0", main = "Sample of 100 postures (fixed-x), n=100 forces per posture.", ylab="Max Residual from reference in last 100ms")
  # settling_time_histogram_for_posture(v[[1]])
  # settling_time_histogram_for_posture(v[[2]])
  tension_settling_scatter(stability_df)
  plot(stability_df$delta_force, stability_df$amortized_velocity_of_force)
  dev.off()
})
