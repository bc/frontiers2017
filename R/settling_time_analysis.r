##' stabilized
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

##' integer midpoint
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

##' Evaluate whether a force trial has stabilized for a given muscle.
##' @param force_trial_df data.frame of numeric values, that includes the reference and measured columns
##' @param desired numeric the desired stabilized value for the vector, if the vector is 'stabilized'
##' @param err numeric the maximum allowable residual for a given value from the desired value.
##' @param muscle muscle of interest string, e.g. 'M0'
##' @return last_value_is_in_range TRUE if last value is in range, false if last value is not in range
force_trial_does_stabilize <- function(force_trial_df, muscle, err) {
  desired <- force_trial_df[1, reference(muscle)]
  last_value <- tail(force_trial_df[, measured(muscle)], 1)
  last_value_is_in_range <- abs(desired - last_value) < err
  return(last_value_is_in_range)
}

##' Get the width of the bounds
##' @param tuple of two integer values
##' @return the integer distance between the values
bound_width <- function(tuple) {
  abs(max(tuple) - min(tuple))
}
##' plot settling time histogram
##' @param stabilized_df stabilized dataframe with column settling_time as vector of integers
##' @param ... parameters passed to histogram function
settling_time_histogram_for_posture <- function(stabilized_df,...) {
  numrows <- nrow(stabilized_df)
  hist(stabilized_df$settling_time, freq = TRUE, xlab = "Settling time (ms)",
    ylab = "Number of Force Trials", main = paste0("Settling time. n_ForceTrials = ",numrows),
    col = "black",...)
}

##' @title fill_initials_into_stabilization_df
##' @param df a stabilization data frame that contains initial_index as a column.
##' @param full_df object for full_df from .rds file
##' @param muscle_of_interest . muscle name string, i.e. 'M0'
##' @return df_filled stabilization dataframe with initial reference values added.
fill_initials_into_stabilization_df <- function(df, full_df, muscle_of_interest) {
  target_prior_indices <- df$initial_index - 1
  reference_values <- as.numeric(lapply(target_prior_indices, get_reference_value,
    full_df, muscle_of_interest))
  df$initial_reference_force <- reference_values
  gc()
  return(df)
}

##' Get reference value from specific row of dataframe
##' TODO test
##' @param index_target int; the index from which we will extract the muscle of interest's reference force
##' @param full_df full dataset from the .rds
##' @param muscle_of_interest string, e.g. 'M0'
get_reference_value <- function(index_target, full_df, muscle_of_interest) {
  return(as.numeric(full_df[index_target, reference(muscle_of_interest)]))
}

##' @title list_of_forces_to_stabilized_df
##' @param forces_list list of force trial dataframes
##' @param err acceptable Newton threshold for settling for tendon force.
##' @return stabilized_df dataframe representing how the list of forces stabilized.
##' @importFrom pbmcapply pbmclapply
list_of_forces_to_stabilized_df <- function(forces_list, err, full_df,
  muscle_of_interest) {
  list_of_stable_dfs <- lapply(forces_list, force_trial_to_stable_index_df, err)
  stabilized_df <- sort_by_initial_index(dcrb(list_of_stable_dfs))
  filled_df <- fill_initials_into_stabilization_df(stabilized_df, full_df, muscle_of_interest)
    stabilized_and_filled_df <- fill_force_velocity_metrics(filled_df)
  return(stabilized_and_filled_df)
}
##' Many Postures to a list of Postures
##' Where 1 Posture has many ForceTrials
##' @param list_of_posture_indices a list of tuples, each with an initial and final index within full_df to extract as a posture.
##' @param full_df full dataset
##' @param column_to_separate_forces string, i.e. "measured_M0"
##' @param err highest acceptable residual from reference force
##' @param last_n_milliseconds integer, used to calculate static stability
##' @param save_rds by deafult it will return the List of List of ForceTrials. Else it will save each posture as an RDS file.
##' @param prefix prefix string for the filenames
##' @return Postures list of list of ForceTrials
##' @importFrom pbapply pblapply
many_postures_to_ForceTrials <- function(list_of_posture_indices, full_df, column_to_separate_forces, err, last_n_milliseconds, save_rds=FALSE, prefix =""){
    if (save_rds){
      pbmclapply(list_of_posture_indices, posture_to_ForceTrials_to_RDS, full_df, column_to_separate_forces, err, last_n_milliseconds, prefix)
    } else {
      fts <- pbmclapply(list_of_posture_indices, posture_to_ForceTrials, full_df, column_to_separate_forces, err, last_n_milliseconds)
      return(fts)
    }
}

##' One list of posture indices to a Posture To RDS
##' Where a Posture has many ForceTrials. use posture_to_ForceTrials if you want the object returned.
##' @param posture_indices tuple, with an initial and final index within full_df to extract as the posture.
##' @param full_df full dataset
##' @param column_to_separate_forces string, i.e. "measured_M0"
##' @param err highest acceptable residual from reference force
##' @param last_n_milliseconds integer, used to calculate static stability
##' @param prefix prefix string to filenames
##' @importFrom pbmcapply pbmclapply
posture_to_ForceTrials_to_RDS <- function(posture_indices, full_df, column_to_separate_forces, err, last_n_milliseconds, prefix = ""){
  ForceTrials_list_for_the_posture <- posture_to_ForceTrials(posture_indices, full_df, column_to_separate_forces, err, last_n_milliseconds)
  first_force_trial <- ForceTrials_list_for_the_posture[[1]]
  adept_coords <- adept_coordinates_from_ForceTrial(first_force_trial)
  force_trial_file_string_with_posture <- paste0(prefix, "force_trial_adept_x_",adept_coords[1], "_adept_y_", adept_coords[2], ".rds")
  save_rds_to_Resilio(ForceTrials_list_for_the_posture, force_trial_file_string_with_posture)

}

##' Signed Max Residual Val
##' TODO test
##' @param range_of_vector the range of a vector (min and max)
##' @return max_residual_val single numeric signed value
signed_max_residual_val <- function(range_of_vector){
  min_bigger_than_max <- diff(abs(range_of_vector)) < 0
  if (min_bigger_than_max > 0) {
    return(range_of_vector[1])
  } else {
    return(range_of_vector[2])
  }
}




##' One list of posture indices to a Posture
##' Where a Posture has many ForceTrials
##' @param posture_indices tuple, with an initial and final index within full_df to extract as the posture.
##' @param full_df full dataset
##' @param column_to_separate_forces string, i.e. "measured_M0"
##' @param err highest acceptable residual from reference force
##' @param last_n_milliseconds integer, used to calculate static stability
##' @return list_of_ForceTrials list of ForceTrials
##' @importFrom pbmcapply pbmclapply
posture_to_ForceTrials <- function(posture_indices, full_df, column_to_separate_forces, err, last_n_milliseconds, muscles_of_interest = muscle_names()){
    posture <- get_forces_list(full_df, posture_indices, column_to_separate_forces)
    fts <- pbmclapply(posture, ForceTrial, full_df, err, last_n_milliseconds, column_to_separate_forces, muscles_of_interest)
  return(fts)
}

##' @title list_of_postures_of_forces_to_stabilized_df
##' @param postures list of postures, each containing a list of force trial dataframes
##' @param err acceptable residual from reference_M0 for settling time
##' @return list_of_stabilized_dfs list of stabilized dataframes.
##' @importFrom parallel mclapply
list_of_postures_of_forces_to_stabilized_df <- function(postures, err,
  full_df, muscle_of_interest) {
  lapply(postures, list_of_forces_to_stabilized_df, err, full_df,
    muscle_of_interest)
}

##' @title fill_force_velocity_metrics
##' compute how the forces change over time, and whether they are fast or slow changes.
##' @param df stabilization data frame with columns initial_reference_force, final_reference_force, settling_time
##' @return df stabilization data frame with new columns delta_force = (final-initial), and amortized_velocity_of_force = (delta_force/settling_time)
fill_force_velocity_metrics <- function(df) {
  df$delta_force <- df$final_reference_force - df$initial_reference_force
  df$amortized_velocity_of_force <- df$delta_force/df$settling_time
  return(df)
}

##' Index of the first value that has stabilized, given two indices.
##' Take two indices of interest and identify whether the first, second or neither converged.
##' When we have narrowed the index of stabilization down to two values,
##' it's got to be one of them (or the time series never stabilized).
##' In a timeseries that stabilized, there are two situations we have
##' to account for. First, if the first of the two indices is stable,
##' then it follows that the second one is also stable.
##' We would return the index of the first bound.
##' If we find that the first element is unstable, but the second
##' element is stable, then we return the upper bound.
##' Else we throw an error proclaiming that neither of the sample
##' converged, and by induction the entire time series never
##' stabilized under the stablization criteria.
##' @param left Logical; whether the left index is stabilized
##' @param right Logical; whether the right index is stabilized
##' @param idx_for_l_and_r the index lower and upper bounds (list of 2 integers). Essentially the bounds.
##' @return idx index of the first TRUE in a list of two Logical values.
first_true_value_idx <- function(left, right, idx_for_l_and_r) {
  stop_if_neither_of_bounds_converged(left,right)
  truth_table <- c(left, right)
  return(idx_for_l_and_r[min(which(truth_table == TRUE))])
}

##' Stop if neight of the bounds converged
##' If we've narrowed it down to two final elements of the timeseries,
##' and there isn't any convergence on either, we respond with an error message.
##' @param left Logical; whether the left index is stabilized
##' @param right Logical; whether the right index is stabilized
stop_if_neither_of_bounds_converged <- function(left,right){
  if (!left & !right) {
    stop("The time series never stabilized under the maximum allowable error threshold")
  }
}
##' Are there no bounds?
##' TODO test
##' @description True when the bounds have converged to a single index.
##' @param idx_bounds must be a tuple vector of lower and upper bounds @param ts_df time series of $value and $index
##' @return true if both elements of bounds are equivalent.
no_bounds <- function(idx_bounds) idx_bounds[1] == idx_bounds[2]

##' Assign new bounds
##' look left or right for the first stable point.
##' TODO test
##' @param midpoint a index value (integer)
##' @param midpoint_is_stable Logical
##' @param bounds a tuple of lower and upper bound indices (integers)
##' @return updated_bounds fixed bounds to reflect whether we should
assign_new_bounds <- function(midpoint, midpoint_is_stable, bounds) {
  stop_if_midpoint_out_of_range(midpoint, bounds)
  bounds_copy <- bounds
  if (midpoint_is_stable) {
    bounds_copy[2] <- midpoint  # move right side in a bit
  } else {
    bounds_copy[1] <- midpoint  # move left side in a bit
  }
  return(bounds_copy)
}

##' Stops the program if the  midpoint is out of range
##' Used in assign_new_bounds
##' @param midpoint a index value (integer)
##' @param bounds a tuple of lower and upper bound indices (integers)
stop_if_midpoint_out_of_range <- function(midpoint, bounds) {
  if(midpoint < bounds[1] || midpoint > bounds[2]){
    stop("The midpoint is out of range")
  }
}

##' Index of first stabilized value of two values of interest
##' The internal call to stablized returns TRUE or FALSE.
##' If left is TRUE, then we should take that one. if right is TRUE we should take that one.
##' @param ts numeric vector of values over time
##' @param bounds a tuple of lower and upper bound indices (integers)
##' @param desired numeric the desired stabilized value for the vector, if the vector is 'stabilized'
##' @param err numeric the maximum allowable residual for a given value from the desired value.
##' @return idx int, index of the first stabilized value within the timeseries ts.
index_of_first_stabilized_val <- function(ts, bounds, desired, err) {
  stop_if_bounds_are_not_len_1(bounds)
  left <- stabilized(ts[bounds[1]:bounds[2]], desired, err)
  right <- stabilized(ts[bounds[2]:bounds[2]], desired, err)
  return(first_true_value_idx(left, right, bounds))
}
stop_if_bounds_are_not_len_1 <- function(bounds){
  if(bound_width(bounds)!=1){
    stop("index_of_first_stabilized_val needs to take in a 2-element timeseries, with bounds that are only 1 index away from one another.")
  }
}

##' stabilized index
##' If length of a vector V is n, and some q exists s.t. v[q:n] is stable,
##' Then any value 1 < x < Q, where x is stable, implies x:N is also stable.
##' bounds = known stability bounds
##' @param ts timeseries vector of numeric values
##' @param desired numeric the desired stabilized value for the vector, if the vector is 'stabilized'
##' @param err numeric the maximum allowable residual for a given value from the desired value.
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

##' Brute force stabilized index
##' Each snip to check is the time series starting at x
##' This algorithm checks for every index, essentially O(n), n=number of samples in the timeseries. Brute force.
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
  stabilized_vec_truth_table <- do.call("c", stabilized_vec)
  stop_if_no_indices_were_stabilized(stabilized_vec_truth_table)
  index <- min(which(stabilized_vec_truth_table == TRUE))
  return(index)
}

stop_if_no_indices_were_stabilized <- function(vector_of_true_false){
  if (sum(vector_of_true_false)==0){
    stop("The time series never stabilized under the maximum allowable error threshold")
  }
}
##' postures grouped by line
##' This is highly specific to the experimental paradigm of realTimeData2017_08_16_13_23_42.rds.
##' @param unique_postures dataframe of $adept_x and @adept_y numeric values (typically with 2 to 3 decimal points, units in millimeters)
##' @param x_fixed_value value of X when Y was being traversed
##' @param y_fixed_value value of Y when X was being traversed
##' @return postures_grouped_by_line list of two dataframes, containing df (postures_x_fixed, and a df of postures_y_fixed), each with columns $adept_x and adept_y with millimeter numeric values.
postures_grouped_by_line <- function(unique_postures, x_fixed_value, y_fixed_value) {
  postures_x_fixed <- unique_postures[unique_postures$adept_x == x_fixed_value,
    ]
  postures_y_fixed <- unique_postures[unique_postures$adept_y == y_fixed_value,
    ]
  return(list(postures_x_fixed, postures_y_fixed))
}
##' discrete_diff
##' Given an array of values, you take the difference between each pair of values, the you return an array of the same size of the original array but -1.
##' @param vector numeric vector of values
##' @return differentiated vector of values, with a displacement of 1 index. length 1 less than input.
discrete_diff <- function(vector) {
  final <- c(vector[-1], 0)
  initial <- vector
  diff_vec <- final - initial
  return(head(diff_vec, length(vector) - 1))
}


##' sort_by_initial_index
##' TODO test
##' @param df data.frame that contains a column initial_index
##' @param df_sorted sorted data.frame by values in a column named 'initial_index'. ascending order.
sort_by_initial_index <- function(df) df[order(df$initial_index), ]



##' force_trial_to_stable_index_df
##' TODO test
##' @param force_trial_df dataframe of the force observations at 1Khz.
##' @inheritParams stabilized_index
##' @return stabilized_index_dataframe cols = idx_i, idx_f, settling_time
force_trial_to_stable_index_df <- function(force_trial_df, err) {
  desired <- tail(force_trial_df$reference_M0, 1)
  stable_idx <- stabilized_index(force_trial_df$measured_M0, desired, err)
  initial_index <- as.integer(first_rowname(force_trial_df))
  df <- data.frame(initial_index = initial_index, final_index = as.integer(last_rowname(force_trial_df)),
    final_reference_force = as.numeric(desired), settling_time = stable_idx)
  gc()
  return(df)
}

######## functions for figure plotting

##' tension_settling_scatter
##' @param settling data frame with columns: settling, initial_tension, final_tension
##' @param ... passed values sent to WVPlots::ScatterHist
##' @return 0 just makes plot of settling~delta_tension
##' if length of a vector V is n, and some q exists s.t. v[q:n] is stable,
##' Then any value 1 < x < Q, where x is stable, implies x:N is also stable.
##' bounds = known stability bounds
##' @export
tension_settling_scatter <- function(stability_df, ...) {
    p <- ggplot(data = stability_df, aes(delta_force, settling_time, col=initial_reference_force))
    p <- p + geom_point(size = 0.03)
    p <- p + xlab("deltaforce_M0 Newtons")
    p <- p + ylab("Settling Time (ms)")
    p <- p + theme_bw()
    return(p)
}

##' abs_value_delta_force_scatter
##' Show settling force for given absolute value of delta force
##' @param settling data frame with columns: settling, initial_tension, final_tension, delta_force
##' @return p ggplot2 plot object
abs_value_delta_force_scatter <- function(stability_df, pointsize){
  stability_df$abs_delta_force <- abs(stability_df$delta_force)
  stability_df$bigger_than_0 <- stability_df$delta_force > 0
  p <- ggplot(data=stability_df, aes(abs_delta_force,settling_time, color=bigger_than_0))
  p <- p + geom_point(size=pointsize)
  p <- p + geom_smooth(method = "lm", se = FALSE)
  p <- p + xlab("|delta force_M0| Newtons")
  p <- p + ylab("Settling Time (ms)")
  p <- p + scale_color_manual(values = c("paleturquoise3", "orchid4"))
  p <- p + theme_bw()
  return(p)
}

##' delta_tension
##' TODO Test
##' @param settling data frame with columns: settling, initial_tension, final_tension
##' @return numric vector of signed differences between prior and initial tensions
delta_tension <- function(settling) {
  return(settling$final_tension - settling$initial_tension)
}

##' Evaluate which of the muscles stabilized to a desired tension within th error threshold.
##' @param force_trial df of force trials with cols including reference_M0, measured_M0
##' @param err highest acceptable error residual from desired tension in same units as measured_MX
##' @return stability_truth_vector list of true/false logicals indicating which of the muscles did stabilized by the end of the time series.
which_muscles_stabilized <- function(force_trial, err, muscles_of_interest) {
  results <- lapply(muscles_of_interest, function(muscle) {
    force_trial_does_stabilize(force_trial, muscle, err)
  })
  stability_truth_vector <- unlist(results)
  return(stability_truth_vector)
}

##' all_muscles_stabilized
##' @param force_trial df of force trials with cols
##' @param err highest acceptable error residual from desired tension
##' @return all_muscles_stabilized true or false
all_muscles_stabilized <- function(force_trial, err, muscles_of_interest) {
  muscle_stabilization_truth_table <- which_muscles_stabilized(force_trial, err, muscles_of_interest)
  stability_true_for_muscles <- sum(muscle_stabilization_truth_table) == length(muscle_stabilization_truth_table)
  return(stability_true_for_muscles)
}

##' mask_settled_force_trials
##' @param list_of_force_trials List of force trials, each a df with cols for each measured/reference force
##' @param err highest acceptable error residual from desired tension
##' @return l_dfs list of force trials without the force trials that did not converge for all muscles
mask_settled_force_trials <- function(list_of_force_trials, err) {
  as.logical(lapply(list_of_force_trials, all_muscles_stabilized, err))
}

##' remove_unsettled_force_trials
##' @param list_of_force_trials List of force trials
##' @param err highest acceptable error residual from desired tension
##' @return l_dfs list of force trials without the force trials that did not converge for all muscles
remove_unsettled_force_trials <- function(list_of_force_trials, err) {
  list_of_force_trials[mask_settled_force_trials(list_of_force_trials, err)]
}


##' Plot remaining force trial fraction as a function of error
##' horizontal line set to exactly 0.99 to indicate the error thresholds that satisfy the metric, where we want at least 99% of the data to remain after the filter.
##' the stabilization_err_99_percentile is used to set the err metric vertical line
##' @param different_errors the X values of error that were tested
##' @param trials_remaining the number of trials that remained when the err was applied as a filter
##' @param total_trials_num the number of total trials, composed of the settled and unsettled force trials.
plot_remaining_force_trial_fraction_as_function_of_err <- function(different_errors,
  trials_remaining, total_trials_num) {
  plot(different_errors, (trials_remaining/total_trials_num)*100, type = "l", xlab = "Maximum Error Threshold (N)",
    ylab = "Force trials that settled (%)", main = paste0("n=", total_trials_num,
      " Force Trials"))
  abline(h = 0.99)
  abline(v = stabilization_err_99_percentile)
}


##' Add Posture to max residual and SD
##' @param row_of_max_residual_and_sd df with 2 cols, with numeric values. signed max residual is the maximum deviation from the desired force
##' @param adept_coords x and y numeric values for posture coordinates
##' @param df dataframe with the posture adept coordinates added as two new columns
add_posture_to_max_residual_and_sd <- function(row_of_max_residual_and_sd, adept_coords){
  cbind(data.frame(adept_x = adept_coords[1], adept_y=adept_coords[2]), row_of_max_residual_and_sd)
}
