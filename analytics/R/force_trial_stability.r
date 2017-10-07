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
  vec_row = data.frame(last_n_milliseconds = last_n_milliseconds, reference = ref, sd = sd(measured_tail), max_residual = max_residual)
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
