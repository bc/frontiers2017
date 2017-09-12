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


##' @param tuple of two integer values
##' @return the integer distance between the values
bound_width <- function(tuple) {
  abs(max(tuple) - min(tuple))
}



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


# @description True when the bounds have converged to a single index.  @param
# idx_bounds must be a tuple vector of lower and upper bounds @param ts_df time
# series of $value and $index
no_bounds <- function(idx_bounds) idx_bounds[1] == idx_bounds[2]

##' @param midpoint a index value (integer)
##' @param midpoint_is_stable Logical
##' @param bounds a tuple of lower and upper bound indices (integers)
##' @return updated_bounds fixed bounds to reflect whether we should
##' look left or right for the first stable point.
assign_new_bounds <- function(midpoint, midpoint_is_stable, bounds) {
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
index_of_first_stabilized_val <- function(ts, bounds, desired, err) {
  left <- stabilized(ts[bounds[1]:bounds[2]], desired, err)
  right <- stabilized(ts[bounds[2]:bounds[2]], desired, err)
  return(first_true_value_idx(left, right, bounds))
}


##' @param ts timeseries vector of numeric values
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
    midpoint_is_stable <- stabilized(ts[midpoint:bounds[2]], desired, err)
    if (bound_width(bounds) == 1) {
      return(index_of_first_stabilized_val(ts, bounds, desired, err))
    }
    bounds <- assign_new_bounds(midpoint, midpoint_is_stable, bounds)
  }
  if (no_bounds(bounds)) {
    return(ifelse(stabilized(ts, desired, err), 1, stop("not stable")))
  }
}

##' @param ts timeseries vector of numeric values
##' @param desired numeric the desired stabilized value for the vector, if the vector is 'stabilized'
##' @param err numeric the maximum allowable residual for a given value from the desired value.
##' @description
##' if length of a vector V is n, and some q exists s.t. v[q:n] is stable,
##' Then any value 1 < x < Q, where x is stable, implies x:N is also stable.
##' bounds = known stability bounds
slow_stabilized_index <- function(ts, desired, err) {
  indices_to_check <- 1:length(ts)
  stabilized_vec <- lapply(indices_to_check, function(x) {
    snip_to_check <- ts[x:length(ts)]
    return(stabilized(snip_to_check, desired, err))
  })
  stabilized_vec <- do.call("c", stabilized_vec)
  return(min(which(stabilized_vec == TRUE)))
}

##' This is highly specific to the experimental paradigm of realTimeData2017_08_16_13_23_42.rds.
##' @param unique_postures dataframe of $adept_x and @adept_y numeric values (typically with 2 to 3 decimal points, units in millimeters)
##' @param x_fixed_value value of X when Y was being traversed
##' @param y_fixed_value value of Y when X was being traversed
##' @return postures_grouped_by_line list of two dataframes, containing df (postures_x_fixed, and a df of postures_y_fixed), each with columns $adept_x and adept_y with millimeter numeric values.
postures_grouped_by_line <- function(unique_postures, x_fixed_value, y_fixed_value){
  postures_x_fixed <- unique_postures[unique_postures$adept_x == x_fixed_value,]
  postures_y_fixed <- unique_postures[unique_postures$adept_y == y_fixed_value,]
  return(list(postures_x_fixed, postures_y_fixed))
}
##' @param vector numeric vector of values'
##' @return differentiated vector of values, with a displacement of 1 index. length 1 less than input.
discrete_diff <- function(vector){
  final <- c(vector[-1], 0)
  initial <- vector
  diff_vec <- final-initial
  return(head(diff_vec,length(vector)-1))
}

########functions for figure plotting

##' @param settling data frame with columns: settling, initial_tension, final_tension
##' @return 0 just makes plot of settling~delta_tension
##' if length of a vector V is n, and some q exists s.t. v[q:n] is stable,
##' Then any value 1 < x < Q, where x is stable, implies x:N is also stable.
##' bounds = known stability bounds
##' @export
##' @importFrom WVPlots ScatterHistC
tension_settling_scatter <- function(settling_df) {
  settling_df$delta_tension <- delta_tension(settling_df)
  WVPlots::ScatterHist(settling_df, "delta_tension", "settling", smoothmethod="lm",
                     title="settling~delta_tension", annot_size = 1)
}

##' @param settling data frame with columns: settling, initial_tension, final_tension
##' @return numric vector of signed differences between prior and initial tensions
delta_tension <- function(settling) {
  return(settling$final_tension - settling$initial_tension)
}
