
require(testthat)
source("../../R/settling_time_analysis.r")
source("../../R/force_trial_stability.r")
source("../../R/generic_tools.r")
source("sample_datasets.r")
sample_vec <- c(1, 1, 1, 1, 1, 1, 4, 8, 9, 4, 5, 3, 2, 3, 2, 3, 2, 3, 3, 3, 3, 3, 3, 3)
sample_vec2 <- c(1, 1, 1, 1, 1, 1, 4, 8, 8, 4, 5, 3, 2, 3, 2, 3, 2, 3, 3, 3, 3, 3, 3, 3)
fake_ft1 <- c(3.895, 3.443, 2.978, 3.032, 2.895, 2.676, 2.423, 2.332, 2.124, 1.876, 1.698, 1.643, 1.445, 1.554, 1.403, 1.454, 1.467, 1.441)
fake_ft2 <- c(3, 2, 2, 2, 2)
fake_ft3 <- c(5, 3, 7, 8, 9)

settling = c(1, 2, 3, 4, 5)
initial_tension = c(22, 45, 50, 47, 19)
final_tension = c(12, 55, 32, 60, 12)
df = data.frame(settling, initial_tension, final_tension)

initial_index = c(3, 5, 4, 1, 2)
value = c(3, 5, 4, 1, 2)
reorder = data.frame(initial_index, value, row.names = initial_index)
initial_index = c(1, 2, 3, 4, 5)
value = c(1, 2, 3, 4, 5)
ordered = data.frame(initial_index, value, row.names = initial_index)


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

#test_that("performance of stabilized_index is acceptable", {
#  library(microbenchmark)
#  replicates = 1000
#  res = microbenchmark(slow_stabilized_index(sample_vec, desired = 3, err = 1),
#    slow_stabilized_index(sample_measured_M0_force_trial, desired = 4, err = 0.5),
#    stabilized_index(sample_vec, desired = 3, err = 1), stabilized_index(sample_measured_M0_force_trial,
#      desired = 4, err = 0.5), times = replicates)
#  pdf("../../../output/settling_time_analysis_performance.pdf", width = 10, height = 10)
#  plot(res)
#  dev.off()
#  expect_equal(1, 1)  #this is here invoke the block & ensure no errors happen in the above code.
#})

context("Testing base functions")

#How do these work?
#Going up, going down, going pos to neg, neg to pos, integers, numeric(floating point)
#TEST
test_that("index_of_first_stabilized_val", {
  expect_equal(index_of_first_stabilized_val(fake_ft1, c(1,18), 1.5, 0.5), 11)
  expect_equal(index_of_first_stabilized_val(fake_ft2, c(1,5), 2, 1), 1)
  #expect_equal(index_of_first_stabilized_val(fake_ft3, c(1,5), 3, 1), throws_error())
  expect_equal(index_of_first_stabilized_val(fake_ft2, c(1,5), 2, 0.1), 2)
  expect_equal(index_of_first_stabilized_val(fake_ft3, c(1,5), 5, 3), 1)
})

#TEST
#test_that("slow_stabilized_index", {
#   expect_equal(slow_stabilized_index(sample_vec, 5, 2), 9)
#})

#TEST
#test_that("force_trial_to_stable_index_df", {
#   expect_equal(something)
#})
#End how do these work?

#These work
#WORKS
test_that("delta_tension", {
   expect_equal(delta_tension(df), c(-10, 10, -18, 13, -7))
})

#WORKS
test_that("no_bounds", {
   expect_equal(no_bounds(c(10, 10)), TRUE)
   expect_equal(no_bounds(c(10, 11)), FALSE)
   expect_equal(no_bounds(c(-10, 11)), FALSE)
   expect_equal(no_bounds(c(-10, -10)), TRUE)
})

#WORKS
test_that("assign_new_bounds", {
  expect_equal(assign_new_bounds(45, TRUE, c(-10, 100)), c(-10, 45))
  expect_equal(assign_new_bounds(45, FALSE, c(-10, 100)), c(45, 100))
  expect_error(assign_new_bounds(101, FALSE, c(-10, 100)))
})

#WORKS
test_that("discrete_diff", {
  expect_equal(discrete_diff(c(1, 2, 3)), c(1, 1))
  expect_equal(discrete_diff(c(10, 10, 10, 10)), c(0, 0, 0))
  expect_equal(discrete_diff(c(10, -10, 10, -10)), c(-20, 20, -20))
  expect_equal(discrete_diff(c(27, 33, 49, 57)), c(6, 16, 8)) 
  expect_equal(discrete_diff(c(27.5, 33, 49, 57)), c(5.5, 16, 8))
})

#WORKS
test_that("Signed max residual val", {
  expect_equal(signed_max_residual_val(c(-14, 13)),-14)
  expect_equal(signed_max_residual_val(c(-10, 13)),13)
  expect_equal(signed_max_residual_val(c(5, 6)), 6)
  expect_equal(signed_max_residual_val(c(5, 7)), 7)
  expect_equal(signed_max_residual_val(c(6, 6)), 6)
  expect_equal(signed_max_residual_val(c(0, 0)), 0)
})

#WORKS
test_that("first_true_value_idx", {
  expect_equal(first_true_value_idx(FALSE, TRUE, c(6, 9)), 9)
  expect_equal(first_true_value_idx(TRUE, TRUE, c(6, 7)), 6)
  expect_that(first_true_value_idx(FALSE, FALSE, c(6, 7)), throws_error())
})

#WORKS
test_that("sort_by_initial_index", {
   expect_equal(sort_by_initial_index(reorder), ordered)
})
#End these work
