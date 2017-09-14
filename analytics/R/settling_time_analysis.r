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
##' plot settling time histogram
##' @param stabilized_df stabilized dataframe with column settling_time as vector of integers
settling_time_histogram_for_posture <- function(stabilized_df)
{
  hist(stabilized_df$settling_time, breaks=20, freq=TRUE, xlab='settling time ms', ylab="Number of force trials", main="Settling times for one posture", col="black")
}
##' @description get the a_i and a_f for each of 100 forces within each of K postures
add_initial_and_final_reference_values <- function(stabilization_dataframe, full_df, muscle_of_interest){
  final_reference_force <- tail(force_trial_df$reference_M0,1)
  initial_reference_force <- get_reference_force_from_index(full_df_path, initial_index - 1, muscle_of_interest = "M0")
}
##' @title fill_initials_into_stabilization_df
##' @param df a stabilization data frame that contains initial_index as a column.
fill_initials_into_stabilization_df <- function(df, full_df, muscle_of_interest) {
  print("Filling initials into new DF")
  target_prior_indices <- df$initial_index - 1
  reference_values <- as.numeric(lapply(target_prior_indices, get_reference_value,
    full_df, muscle_of_interest))
  df$initial_reference_force <- reference_values
  gc()
  return(df)
}


##' @param index_target int; the index from which we will extract the muscle of interest's reference force
##' @inheritFrom get_reference_force_from_index force_trials_per_posture
# dt_approach <- get_reference_force_from_index(data_location, idx=index_target, muscle_of_interest = "M0")
##' @TODO test
get_reference_value <- function(index_target, full_df, muscle_of_interest){
  return(as.numeric(full_df[index_target,reference(muscle_of_interest)]))
}

##' @title fill_force_velocity_metrics
##' compute how the forces change over time, and whether they are fast or slow changes.
##' @param df stabilization data frame with columns initial_reference_force, final_reference_force, settling_time
##' @return df stabilization data frame with new columns delta_force = (final-initial), and amortized_velocity_of_force = (delta_force/settling_time)
fill_force_velocity_metrics <- function(df){
  df$delta_force <- df$final_reference_force - df$initial_reference_force
  df$amortized_velocity_of_force <- df$delta_force / df$settling_time
  return(df)
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


##' sort_by_initial_index
##' @param df data.frame that contains a column initial_index
##' @param df_sorted sorted data.frame by values in initial_index. ascending order.
sort_by_initial_index <- function(df) df[order(df$initial_index),]



##' force_trial_to_stable_index_df
##' @param force_trial_df dataframe of the force observations at 1Khz.
##' @inheritParams stabilized_index
##' @return stabilized_index_dataframe cols = idx_i, idx_f, settling_time
force_trial_to_stable_index_df <- function(force_trial_df, full_df_path, err) {
  desired <- tail(force_trial_df$reference_M0,1)
  stable_idx <- stabilized_index(force_trial_df$measured_M0, desired, err)
  initial_index <- as.integer(first_rowname(force_trial_df))
  df <- data.frame(initial_index = initial_index, final_index = last_rowname(force_trial_df), final_reference_force = as.numeric(desired),
    settling_time = stable_idx)
  gc()
  return(df)
}

##' Rbind multiple dataframes in list
##' @param list_of_dfs data.frame that contains a column initial_index
##' @return df combined large dataframe
rbind_dfs <- function(list_of_dfs) do.call('rbind', list_of_dfs)

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
