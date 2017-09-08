require(testthat)
sample_vec <- c(1, 1, 1, 1, 1, 1, 4, 8, 9, 4, 5, 3, 2, 3, 2, 3, 2, 3, 3, 3, 3, 3,
  3, 3)

test_that("All indices to the right are within error bounds", {
  expect_that(stabilized(sample_vec, 3, 1), is_false())
  expect_that(stabilized(sample_vec[12:length(sample_vec)], 3, 1), is_true())
  expect_that(stabilized(sample_vec[7], 3, 1), is_true())
  expect_that(stabilized(c(2.5), 2, 1), is_true())
  expect_that(stabilized(c(1.00001), 0, 1), is_false())
  expect_that(stabilized(c(-1), 1, 1), is_false())
  expect_that(stabilized(c(-1), 1, -1), throws_error())
})
stabilized <- function(vector, desired, err) {
  if (err <= 0) {
    stop("err must be a positive non-zero number")
  }
  residuals <- desired - vector
  # if there are no points that are outside the allowable error
  return(!sum(abs(residuals) > err) > 0)
}





test_that("floor of midpoint gets correct index", {
  expect_equal(floor_of_midpoint(-10, 100), 45)
  expect_equal(floor_of_midpoint(-10, 10), 0)
  expect_equal(floor_of_midpoint(0, 10), 5)
  expect_equal(floor_of_midpoint(-10.241, 1.7521), -5)
  expect_equal(floor_of_midpoint(-12.241, 1.7521), -6)
})
floor_of_midpoint <- function(lower, upper) {
  return(floor((lower + upper)/2))
}




test_that("inter_range_distance", {
  expect_equal(inter_range_distance(c(1, 5)), 4)
})
inter_range_distance <- function(tuple) {
  abs(max(tuple) - min(tuple))
}


test_that("floor of midpoint gets correct index", {
  expect_equal(first_true_value_idx(FALSE, TRUE, c(6, 7)), 7)
  expect_equal(first_true_value_idx(TRUE, TRUE, c(6, 7)), 6)
  expect_that(first_true_value_idx(FALSE, FALSE, c(6, 7)), throws_error())
})


# @return idx index of the first TRUE in a list of two Logical values.
first_true_value_idx <- function(left, right, idx_for_l_and_r) {
  if (!left & !right) {
    stop("The time series never stabilized under the maximum allowable error threshold")
  }
  truth_table <- c(left, right)
  return(idx_for_l_and_r[min(which(truth_table == TRUE))])
}

# @description True when the bounds have converged to a single index.  @param
# idx_bounds must be a tuple vector of lower and upper bounds @param ts_df time
# series of $value and $index
no_bounds <- function(idx_bounds) idx_bounds[1] == idx_bounds[2]

##' err is maximum allowable error
##' desired is the desired steady state value.
##' @description
##' if length of a vector V is n, and some q exists s.t. v[q:n] is stable,
##' Then any value 1 < x < Q, where x is stable, implies x:N is also stable.
index_when_timeseries_stabilized <- function(ts, desired, err) {
  lowest_stable_index_so_far <- tail(ts, 1)
  known_stability_bounds <- c(1, length(ts))
  message(paste("\n Starting Bounds:", known_stability_bounds[1], "< idx <", known_stability_bounds[2]))
  while (inter_range_distance(known_stability_bounds) != 0) {
    midpoint_idx <- floor_of_midpoint(known_stability_bounds[1], known_stability_bounds[2])
    # Test whether midpoint_idx is stable
    midpoint_is_stable <- stabilized(ts[midpoint_idx:known_stability_bounds[2]],
      desired, err)
    if (inter_range_distance(known_stability_bounds) == 1) {
      left <- stabilized(ts[known_stability_bounds[1]:known_stability_bounds[2]],
        desired, err)
      right <- stabilized(ts[known_stability_bounds[2]:known_stability_bounds[2]],
        desired, err)
      return(first_true_value_idx(left, right, known_stability_bounds))
    }
    if (midpoint_is_stable) {
      message(paste("index", midpoint_idx, "is stable"))
      # If it's stable, move bounds to left to look for more ambitious indices
      lowest_stable_index_so_far <- known_stability_bounds[1]
      known_stability_bounds[2] <- midpoint_idx  # move right side in a bit
    } else {
      message(paste("index", midpoint_idx, "is unstable"))
      # If it's unstable, move bounds to right to look for more conservative indices
      lowest_stable_index_so_far <- known_stability_bounds[1]
      known_stability_bounds[1] <- midpoint_idx  # move left side in a bit
    }
    message(paste("\n ", known_stability_bounds[1], "< idx <", known_stability_bounds[2]))
  }
}
