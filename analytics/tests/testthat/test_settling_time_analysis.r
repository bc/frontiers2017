require(testthat)
source("../../R/settling_time_analysis.r")
source("sample_datasets.r")
sample_vec <- c(1, 1, 1, 1, 1, 1, 4, 8, 9, 4, 5, 3, 2, 3, 2, 3, 2, 3, 3, 3, 3, 3,
  3, 3)

test_that("stabilized", {
  expect_that(stabilized(sample_vec, 3, 1), is_false())
  expect_that(stabilized(sample_vec[12:length(sample_vec)], 3, 1), is_true())
  expect_that(stabilized(sample_vec[7], 3, 1), is_true())
  expect_that(stabilized(c(2.5), 2, 1), is_true())
  expect_that(stabilized(c(1.00001), 0, 1), is_false())
  expect_that(stabilized(c(-1), 1, 1), is_false())
  expect_that(stabilized(c(-1), 1, -1), throws_error())
})

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


test_that("postures_grouped_by_line", {
  unique_postures <- data.frame(adept_x = c(-516.314298, -531.478918, -525.80549,
    -525, -525, -525, -525, -525, -525, -525, -525), adept_y = c(68, 68, 68,
    63.360715, 63.522459, 61.802527, 72.122261, 65.948095, 72.264025, 62.633837,
    68.007593), row.names = c(81125159, 81206563, 81288007, 81369528, 81450638,
    81531857, 81613207, 81694520, 81775837, 81857174, 81938463))
  x_fixed_value <- -525
  y_fixed_value <- 68
  line_list <- postures_grouped_by_line(unique_postures, x_fixed_value, y_fixed_value)
  expect_equal(line_list[[1]], unique_postures[4:11, ])
  expect_equal(line_list[[2]], unique_postures[1:3, ])
})


test_that("discrete_diff", {
  expect_equal(discrete_diff(c(1, 2, 3)), c(1, 1))
  expect_equal(discrete_diff(c(10, 10, 10, 10)), c(0, 0, 0))
  expect_equal(discrete_diff(c(10, -10, 10, -10)), c(-20, 20, -20))
})

test_that("one can remove nonstabilized force trials", {
  print('attempting to remove nonstabilized force trials')
  posture_samples_n_100_fix_x <- rds_from_package_extdata("posture_samples_n_100_fix_x.rds")
  test_example_force_trial <- posture_samples_n_100_fix_x[[1]][[1]]
  first_posture_stablizes <- force_trial_does_stabilize(test_example_force_trial, muscle="M0", err=0.5)
  too_stringent_yields_false <- force_trial_does_stabilize(test_example_force_trial, muscle="M0", err=0.01)
  expect_true(first_posture_stablizes)
  expect_false(too_stringent_yields_false)
  #try it for many postures
  all_force_trials <- unlist(posture_samples_n_100_fix_x, recursive=FALSE)

  mask_for_posture <- function(list_of_force_trials, muscle, err){
    as.logical(lapply(list_of_force_trials, force_trial_does_stabilize, muscle, err))
  }
  indices_of_nonstabilized_force_trials <- which(!mask_for_posture)
  num_force_trials_that_did_not_stabilize <- length(indices_of_nonstabilized_force_trials)
  print(paste("num_force_trials_that_did_not_stabilize=",num_force_trials_that_did_not_stabilize))
  browser()
})

test_that("can_eval_stabilize_idx_to_2_postures", {
  # load a list of postures fixed in X, each a list of force trials DFs.
  print("Loading full_df. Expect 2'")
  full_df <- readRDS("~/Resilio Sync/data/realTimeData2017_08_16_13_23_42.rds")
  print("Loading precomputed posture samples")
  posture_samples_n_100_fix_x <- rds_from_package_extdata("posture_samples_n_100_fix_x.rds")

  v <- list_of_postures_of_forces_to_stabilized_df(posture_samples_n_100_fix_x[70:100], full_df_path = data_location,
    err = 0.5, full_df, muscle_of_interest = "M0")



  ##' get_reference_to_variance_relationship_df
  ##' @param force_list list of force time trials
  ##' @param last_n_milliseconds the number of tail milliseconds from which we should calculate the settled standard deviation.
  ##' @param muscle the muscle of interest in a string e.g. 'M0'
  ##' @return reference_standard_deviation df with reference (desired) muscle tension, variance, maximum residual
  ##' @importFrom base rbind
  get_reference_to_variance_relationship_df <- function(force_list, last_n_milliseconds,
    muscle) {
    ref_sd_df <- do.call("rbind", lapply(forces, function(x) {
      ref <- tail(x, 1)[, reference(muscle)]
      measured_tail <- tail(x, last_n_milliseconds)[, measured(muscle)]
      c(ref, sd(measured_tail), max(abs(range(measured_tail) - ref)))
    }))
    colnames(ref_sd_df) <- c("reference", "sd_of_tail", "max_residual")
    rownames(ref_sd_df) <- NULL
    return(ref_sd_df)
  }
  # get_reference_to_variance_relationship_df(x, 30, 'M0')}) a <- do.call('rbind',)
  # plot(tsd, xlab='M0 reference force (N)', ylab = 'SD(last 100 ms of force
  # trial)', pch=19)

  stability_df <- do.call("rbind", list_of_postures_of_forces_to_stabilized_df(posture_samples_n_100_fix_x[1:2][-13][-14],
    full_df_path = data_location, err = 0.5, full_df, muscle_of_interest = "M0"))

  ##' force_trial_to_stable_metrics
  ##' @param force_trial force trial time series
  ##' @param last_n_milliseconds the number of tail milliseconds from which we should calculate the settled standard deviation.
  ##' @param muscle the muscle of interest in a string e.g. 'M0'
  ##' @return vec_row dataframe row with reference, and max residual
  force_trial_to_stable_metrics <- function(force_trial, last_n_milliseconds, muscle) {
    ref <- tail(force_trial, 1)[, reference(muscle)]
    measured_tail <- tail(force_trial, last_n_milliseconds)[, measured(muscle)]
    max_residual <- max(abs(range(measured_tail) - ref))
    vec_row = data.frame(reference = ref, sd = sd(measured_tail), max_residual = max_residual)
    return(vec_row)
  }
  ##' posture_list_to_stability_metrics_df_rows
  ##' @param list_of_force_trials each a force_trial time series
  ##' @param last_n_milliseconds the number of tail milliseconds from which we should calculate the settled standard deviation.
  ##' @param muscle the muscle of interest in a string e.g. 'M0'
  ##' @return stability_df_rows df with reference (desired) muscle tension, variance, maximum residual
  posture_list_to_stability_metrics_df_rows <- function(list_of_force_trials, last_n_milliseconds,
    muscle) {
    reference_df_rows <- lapply(list_of_force_trials, force_trial_to_stable_metrics,
      last_n_milliseconds, muscle)
    return(do.call("rbind", reference_df_rows))
  }

  stability_metrics_df <- do.call("rbind", lapply(posture_samples_n_100_fix_x[1:100], posture_list_to_stability_metrics_df_rows, last_n_milliseconds = 100, muscle = "M0"))
  hist(stability_metrics_df[,3], breaks=50, col='black', xlim=c(0, 2), xlab="Maximum residual from desired force in last 100ms", main = "Sample of 100 postures (fixed-x), n=100 forces per posture.")
  plot(stability_metrics_df$reference, stability_metrics_df$max_residual, col = alpha('black', 0.15), pch=20, xlab="Reference force for M0", main = "Sample of 100 postures (fixed-x), n=100 forces per posture.", ylab="Max Residual from reference in last 100ms")
  # settling_time_histogram_for_posture(v[[1]])
  # settling_time_histogram_for_posture(v[[2]])
  tension_settling_scatter(stability_df)

  browser()
})
