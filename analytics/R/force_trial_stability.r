##' get_reference_to_variance_relationship_df
##' @param force_list list of force time trials
##' @param last_n_milliseconds the number of tail milliseconds from which we should calculate the settled standard deviation.
##' @param muscle the muscle of interest in a string e.g. 'M0'
##' @return reference_standard_deviation df with reference (desired) muscle tension, variance, maximum residual
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
##' force_trial_to_stable_metrics
##' @param force_trial force trial time series
##' @param last_n_milliseconds the number of tail milliseconds from which we should calculate the settled standard deviation.
##' @param muscle the muscle of interest in a string e.g. 'M0'
##' @return vec_row dataframe row with reference, and max residual
force_trial_to_stable_metrics <- function(force_trial, last_n_milliseconds, muscle) {
  ref <- tail(force_trial, 1)[, reference(muscle)]
  measured_tail <- tail(force_trial, last_n_milliseconds)[, measured(muscle)]
  max_residual <- max(abs(range(measured_tail) - ref))
  vec_row = data.frame(last_n_milliseconds = last_n_milliseconds, reference = ref,
    sd = sd(measured_tail), max_residual = max_residual)
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

##' How many force trials meet the spec, if the spec is a certain envelope size for error?
##' Produces a plot of remaining % as a function of increasing err threshold. Draws a hardcoded line for the 99%
##' @param force_trials a list of raw force timeseries_DFs. has_settled has not been applied yet
##' @param err_length.out integer, the resolution along the X axis by which diff err thresholds will be evaluated
##' @importFrom parallel mclapply
try_different_stability_thresholds <- function(force_trials, err_length.out = 20) {
  different_errors <- seq(0.1, 1, length.out = err_length.out)
  num_remaining_force_trials <- unlist(mclapply(different_errors, function(err) {
    length(remove_unsettled_force_trials(force_trials, err))
  }))
  plot_remaining_force_trial_fraction_as_function_of_err(different_errors, num_remaining_force_trials,
    length(force_trials))
}


##' Indices from a Posture DF row'
##'@param posture_df_row a dataframe with initial and final columns
##'@return indices a list of two elements, the initial and final index.
indices_from_posture_df_row <- function(posture_df_row) c(posture_df_row[["initial"]],
  posture_df_row[["final"]])

##' Turn a dataframe of collected observations, with square input to reference, into formal ForceTrial objects
##' Works with stabilized or nonstabilized force trials. It will add attributes of stabilization only for those that meet the err criteria
##' @param df_of_postures dataframe with columns including the initial and final indices in reference to each posture group collected.
##' @param full_df dataframe object of the full dataset collected, from which the indices will extract the sections.
##' @return list_of_force_trials A list containing a list (postures) of many ForceTrial objects.
##' @importFrom pbmcapply pbmclapply
posture_start_finish_indices_to_L_of_ForceTrials <- function(df_of_postures, full_df,
  data_location, err) {
  pbmclapply(df_to_list_of_rows(df_of_postures), function(row) {
    indices <- indices_from_posture_df_row(row)
    return(lapply(get_forces_list(full_df, indices, column_to_separate_forces = "reference_M0"),
      ForceTrial, data_location, full_df, err))
  })
}
