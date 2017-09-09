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
  mbm = microbenchmark(slow_stabilized_index(sample_vec, desired = 3, err = 1),
    slow_stabilized_index(sample_measured_M0_force_trial, desired = 4, err = 0.5),
    stabilized_index(sample_vec, desired = 3, err = 1), stabilized_index(sample_measured_M0_force_trial,
      desired = 4, err = 0.5), times = replicates)
  pdf("../../../output/settling_time_analysis_performance.pdf", width = 10, height = 10)
  if (require("ggplot2")) {
    autoplot(res) + title(paste(replicates, "replicates"))
  }
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
  expect_equal(discrete_diff(c(1,2,3)),c(1,1))
  expect_equal(discrete_diff(c(10,10,10,10)),c(0,0))
  expect_equal(discrete_diff(c(10,-10,10,-10)),c(-20,20,-20))
})
