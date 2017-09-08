require(testthat)
sample_vec <- c(1, 1, 1, 1, 1, 1, 4, 8, 9, 4, 5, 3, 2, 3, 2, 3, 2, 3, 3, 3, 3, 3,
  3, 3)


##' @param vector vector of numeric values, that change by a constant interval of time.
##' @param desired numeric the desired stabilized value for the vector, if the vector is 'stabilized'
##' @param err numeric the maximum allowable residual for a given value from the desired value.
##' This defines the threshold
##' @return stabilized Logical, whether or not the value is stabilized
stabilized <- function(vector, desired, err) {
  if (err <= 0) {
    stop("err must be a positive non-zero number")
  }
  residuals <- desired - vector
  # if there are no points that are outside the allowable error
  return(!sum(abs(residuals) > err) > 0)
}
test_that("All indices to the right are within error bounds", {
  expect_that(stabilized(sample_vec, 3, 1), is_false())
  expect_that(stabilized(sample_vec[12:length(sample_vec)], 3, 1), is_true())
  expect_that(stabilized(sample_vec[7], 3, 1), is_true())
  expect_that(stabilized(c(2.5), 2, 1), is_true())
  expect_that(stabilized(c(1.00001), 0, 1), is_false())
  expect_that(stabilized(c(-1), 1, 1), is_false())
  expect_that(stabilized(c(-1), 1, -1), throws_error())
})

##' @param tuple_of_lower_and_upper a tuple of two integers, denoting the lower and upper bound
##' @return midpoint an integer that is between the lower and upper bound.
##' @description
##' For evenly lengthed tuples, this will give the floor value,
##' but for oddly lengthed tuples it will give the center value.
integer_midpoint <- function(tuple_of_lower_and_upper) {
  lower <- tuple_of_lower_and_upper[1]
  upper <- tuple_of_lower_and_upper[2]
  return(floor((lower + upper)/2))
}
test_that("floor of midpoint gets correct index", {
  expect_equal(integer_midpoint(c(-10, 100)), 45)
  expect_equal(integer_midpoint(c(-10, 10)), 0)
  expect_equal(integer_midpoint(c(0, 10)), 5)
  expect_equal(integer_midpoint(c(-10.241, 1.7521)), -5)
  expect_equal(integer_midpoint(c(-12.241, 1.7521)), -6)
})




##' @param tuple of two integer values
##' @return the integer distance between the values
bound_width <- function(tuple) {
  abs(max(tuple) - min(tuple))
}
test_that("bound_width", {
  expect_equal(bound_width(c(1, 5)), 4)
})


##' @param left Logical; whether the left index is stabilized
##' @param right Logical; whether the right index is stabilized
##' @param idx_for_l_and_r the index lower and upper bounds (list of 2 integers)
##' @return idx index of the first TRUE in a list of two Logical values.
first_true_value_idx <- function(left, right, idx_for_l_and_r) {
  if (!left & !right) {
    stop("The time series never stabilized under the maximum allowable error threshold")
  }
  truth_table <- c(left, right)
  return(idx_for_l_and_r[min(which(truth_table == TRUE))])
}
test_that("first_true_value_idx", {
  expect_equal(first_true_value_idx(FALSE, TRUE, c(6, 7)), 7)
  expect_equal(first_true_value_idx(TRUE, TRUE, c(6, 7)), 6)
  expect_that(first_true_value_idx(FALSE, FALSE, c(6, 7)), throws_error())
})

# @description True when the bounds have converged to a single index.  @param
# idx_bounds must be a tuple vector of lower and upper bounds @param ts_df time
# series of $value and $index
no_bounds <- function(idx_bounds) idx_bounds[1] == idx_bounds[2]

##' @param midpoint a index value (integer)
##' @param midpoint_is_stable Logical
##' @param bounds a tuple of lower and upper bound indices (integers)
##' @return updated_bounds fixed bounds to reflect whether we should
##' look left or right for the first stable point.
assign_new_bounds <- function(midpoint, midpoint_is_stable, bounds){
  bounds_copy <- bounds
  if (midpoint_is_stable) {
    bounds_copy[2] <- midpoint  # move right side in a bit
  } else {
    bounds_copy[1] <- midpoint  # move left side in a bit
  }
  return(bounds_copy)
}

##' @param ts numeric vector of values over time
##' @param bounds a tuple of lower and upper bound indices (integers)
##' @param desired numeric the desired stabilized value for the vector, if the vector is 'stabilized'
##' @param err numeric the maximum allowable residual for a given value from the desired value.
##' @return idx int, index of the first stabilized value within the timeseries ts.
index_of_first_stabilized_val <- function(ts, bounds, desired, err){
  left <- stabilized(ts[bounds[1]:bounds[2]],
    desired, err)
  right <- stabilized(ts[bounds[2]:bounds[2]],
    desired, err)
  return(first_true_value_idx(left, right, bounds))
}


##' @param ts timeseries of numeric values
##' @param desired numeric the desired stabilized value for the vector, if the vector is 'stabilized'
##' @param err numeric the maximum allowable residual for a given value from the desired value.
##' @description
##' if length of a vector V is n, and some q exists s.t. v[q:n] is stable,
##' Then any value 1 < x < Q, where x is stable, implies x:N is also stable.
##' bounds = known stability bounds
stabilized_index <- function(ts, desired, err) {
  bounds <- c(1, length(ts))
  while (bound_width(bounds) != 0) {
    midpoint <- integer_midpoint(bounds)
    midpoint_is_stable <- stabilized(ts[midpoint:bounds[2]],
      desired, err)
    if (bound_width(bounds) == 1) {
      return(index_of_first_stabilized_val(ts, bounds, desired, err))
    }
    bounds <- assign_new_bounds(midpoint, midpoint_is_stable, bounds)
  }
  if (no_bounds(bounds)) {
      return(ifelse(stabilized(ts,desired,err), 1, stop("not stable")))
  }
}

test_that("A stable timeseries will return its convergence index", {
  expect_equal(stabilized_index(c(1,1,5,1,1,1,1,3,3), 3, 1), 8)
  expect_equal(stabilized_index(c(1,1,5,1,1,1,1,-3,-3), -3, 1), 8)
  expect_equal(stabilized_index(c(-3), -3, 1), 1)
  expect_equal(stabilized_index(c(-2.888), -3, 1), 1)
  expect_that(stabilized_index(c(1,1), -3, 1), throws_error())
  expect_that(stabilized_index(c(-1,-1), -3, 1), throws_error())
  expect_equal(stabilized_index(sample_vec, 3, 1), 12)
  expect_equal(stabilized_index(sample_vec, 3, 1), 12)
  expect_equal(stabilized_index(sample_measured_M0_force_trial, 4, 0.5), 207)
})

indices_to_check <- 1:length(sample_measured_M0_force_trial)
stabilized_vec <- lapply(indices_to_check, function(x) {
  snip_to_check <- sample_measured_M0_force_trial[-x:0]
  return(stabilized(snip_to_check, 4, 0.5))
})
stabilized_vec <- do.call('c',stabilized_vec)
