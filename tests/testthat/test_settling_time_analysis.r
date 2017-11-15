
require(testthat)
source("../../R/settling_time_analysis.r")
source("../../R/force_trial_stability.r")
source("../../R/generic_tools.r")


sample_vec <- c(1, 1, 1, 1, 1, 1, 4, 8, 9, 4, 5, 3, 2, 3, 2, 3, 2, 3, 3, 3, 3, 3, 3, 3)
sample_vec2 <- c(1, 1, 1, 1, 1, 1, 4, 8, 8, 4, 5, 3, 2, 3, 2, 3, 2, 3, 3, 3, 3, 3, 3, 3)
fake_ft1 <- c(3.895, 3.443, 2.978, 3.032, 2.895, 2.676, 2.423, 2.332, 2.124, 1.876, 1.698, 1.643, 1.445, 1.554, 1.403, 1.454, 1.467, 1.441)
fake_ft2 <- c(3, 2, 2, 2, 2)
fake_ft3 <- c(5, 3, 5, 7, 8, 9)
settling  <- c(1, 2, 3, 4, 5)
initial_tension1  <- c(22, 45, 50, 47, 19)
final_tension1  <- c(12, 55, 32, 60, 12)
delta1  <- data.frame(settling, initial_tension1, final_tension1)
initial_tension2  <- c(22.5, 45.5, 50, 47.95, 19)
final_tension2  <- c(12.5, 55.6, 32.97, 60.76, 12)
delta2  <- data.frame(settling, initial_tension2, final_tension2)
initial_index  <- c(3, 5, 4, 1, 2)
value  <- c(3, 5, 4, 1, 2)
reorder  <- data.frame(initial_index, value, row.names = initial_index)
initial_index  <- c(1, 2, 3, 4, 5)
value = c(1, 2, 3, 4, 5)
ordered <- data.frame(initial_index, value, row.names = initial_index)
list_of_ft = read_rds_from_package_extdata('force_trial_adept_x_-527.463336_adept_y_68.rds')
sample_measured_M0_force_trial <- read_rds_from_package_extdata('sample_measured_M0_force_trial.rds')

ft = ft_to_df(list_of_ft[[1]])
test_df1 = data.frame(1, 803, 3.170629, 194)
test_df2 = data.frame(1, 803, 3.170629, 108)
test_df3 = data.frame(1, 803, 3.170629, 476)
colnames(test_df1) <- c("initial_index", "final_index", "final_reference_force", "settling_time")
colnames(test_df2) <- c("initial_index", "final_index", "final_reference_force", "settling_time")
colnames(test_df3) <- c("initial_index", "final_index", "final_reference_force", "settling_time")

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
  replicates = 10
  res = microbenchmark(slow_stabilized_index(sample_vec, desired = 3, err = 1),
    slow_stabilized_index(sample_measured_M0_force_trial, desired = 4, err = 0.5),
    stabilized_index(sample_vec, desired = 3, err = 1), stabilized_index(sample_measured_M0_force_trial,
      desired = 4, err = 0.5), times = replicates)
  pdf("../../output/settling_time_analysis_performance.pdf", width = 10, height = 10)
  plot(res)
  dev.off()
  expect_equal(1, 1)  #this is here invoke the block & ensure no errors happen in the above code.
})

context("Testing base functions")

test_that("force_trial_to_stable_index_df", {
  expect_equal(force_trial_to_stable_index_df(ft, 0.5), test_df1)
  expect_equal(force_trial_to_stable_index_df(ft, 1.6), test_df2)
  expect_equal(force_trial_to_stable_index_df(ft, 0.2), test_df3)
  expect_error(force_trial_to_stable_index_df(ft, 0.1), "The time series never stabilized under the maximum allowable error threshold")
})

context("Slow Stabilized Index (bruteforce)")
test_that("slow_stabilized_index throws error on nonstabilized vector", {
  expect_error(slow_stabilized_index(c(1.1, 1.2), -3.1, 1.1),"The time series never stabilized under the maximum allowable error threshold")
  expect_error(slow_stabilized_index(c(-1, -1), -3, 1), "The time series never stabilized under the maximum allowable error threshold")
})
test_that("slow_stabilized_index", {
  expect_true(slow_stabilized_index(c(-3), -3, 1) == stabilized_index(c(-3), -3, 1))
  expect_true(slow_stabilized_index(c(-2.888), -3, 1) == stabilized_index(c(-2.888), -3, 1))
  expect_equal(slow_stabilized_index(c(-2,3), 1, 2), 2)
  expect_equal(slow_stabilized_index(c(1.1,1.2), 1, 0.2), 1)
  expect_true(slow_stabilized_index(sample_vec, 3, 1) == stabilized_index(sample_vec, 3, 1))
  expect_true(slow_stabilized_index(sample_measured_M0_force_trial, 4, 0.5) == slow_stabilized_index(sample_measured_M0_force_trial, 4, 0.5))
  expect_equal(slow_stabilized_index(fake_ft1, 1.4, 0.07), 15)
  expect_true(slow_stabilized_index(fake_ft1, 1.4, 0.07) == stabilized_index(fake_ft1, 1.4, 0.07))
})
context('slow_stabilized_index when it never stabilizes')
test_that('Expect an error message when passing in a vector of length two that never stabilizes', {
  expect_error(stabilized_index(c(1.1, 1.2), -3.1, 1.1), "The time series never stabilized under the maximum allowable error threshold")
  expect_error(slow_stabilized_index(c(1.1, 1.2), -3.1, 1.1), "The time series never stabilized under the maximum allowable error threshold")
})

test_that("index_of_first_stabilized_val", {
  expect_equal(index_of_first_stabilized_val(fake_ft1, c(10,11), 1.5, 0.5), 10)
  expect_equal(index_of_first_stabilized_val(fake_ft1, c(10,11), 1.5, 0.2), 11)
  expect_error(index_of_first_stabilized_val(fake_ft1, c(10,11), 1.5, 0.1))
  expect_equal(index_of_first_stabilized_val(fake_ft3, c(4,5), 6, 2), 4)
  expect_equal(index_of_first_stabilized_val(fake_ft3, c(3,4), 7, 1), 4)
  expect_error(index_of_first_stabilized_val(fake_ft3, c(2,3), 7, 1))
})

test_that("delta_tension", {
  expect_equal(delta_tension(delta1), c(-10, 10, -18, 13, -7))
  expect_equal(delta_tension(delta2), c(-10, 10.1, -17.03, 12.81, -7))
})

test_that("no_bounds", {
  expect_equal(no_bounds(c(10, 10)), TRUE)
  expect_equal(no_bounds(c(10, 11)), FALSE)
  expect_equal(no_bounds(c(-10, 11)), FALSE)
  expect_equal(no_bounds(c(-10, -10)), TRUE)
})

test_that("assign_new_bounds", {
  expect_equal(assign_new_bounds(45, TRUE, c(-10, 100)), c(-10, 45))
  expect_equal(assign_new_bounds(45, FALSE, c(-10, 100)), c(45, 100))
  expect_error(assign_new_bounds(101, FALSE, c(-10, 100)))
})

test_that("discrete_diff", {
  expect_equal(discrete_diff(c(1, 2, 3)), c(1, 1))
  expect_equal(discrete_diff(c(10, 10, 10, 10)), c(0, 0, 0))
  expect_equal(discrete_diff(c(10, -10, 10, -10)), c(-20, 20, -20))
  expect_equal(discrete_diff(c(27, 33, 49, 57)), c(6, 16, 8))
  expect_equal(discrete_diff(c(27.5, 33, 49, 57)), c(5.5, 16, 8))
})

test_that("Signed max residual val", {
  expect_equal(signed_max_residual_val(c(-14, 13)),-14)
  expect_equal(signed_max_residual_val(c(-10, 13)),13)
  expect_equal(signed_max_residual_val(c(5, 6)), 6)
  expect_equal(signed_max_residual_val(c(5, 7)), 7)
  expect_equal(signed_max_residual_val(c(6, 6)), 6)
  expect_equal(signed_max_residual_val(c(0, 0)), 0)
})

test_that("first_true_value_idx", {
  expect_equal(first_true_value_idx(FALSE, TRUE, c(6, 9)), 9)
  expect_equal(first_true_value_idx(TRUE, TRUE, c(6, 7)), 6)
  expect_that(first_true_value_idx(FALSE, FALSE, c(6, 7)), throws_error())
})

test_that("sort_by_initial_index", {
   expect_equal(sort_by_initial_index(reorder), ordered)
})
