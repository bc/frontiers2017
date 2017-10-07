##' Instantiate a Force Trial Structure Object
##' We chose M0 as the representative muscle to converge because from all
##' experiments it appeared to have a representative response for the other muscles
##' we remove the angle and adept columns because they are unused and redundant, respectively
##' @param adept_x Numeric position of adept (positive or negative)
##' @param adept_y Numeric position of adept (positive or negative)
##' @param timeseries_df dataframe of the time series with all 39 columns
##' @param err max allowable residual from desired reference force.
##' @param last_n_milliseconds number of milliseconds to base the stability metrics off.
##' @return forcetrial_structure_object the object with all relevant attributes and stability metrics
ForceTrial <- function(timeseries_df, full_df_path, full_df, err, last_n_milliseconds = 100,
  muscle_of_interest = "M0") {
  if (1 %in% timeseries_df$robot_flag) {
    print("robot was moving during force trial")
  }
  if (0 %in% timeseries_df$robot_flag) {
    print("robot was not initialized correctly during force trial")
  }
  all_muscles_stabilized_by_last_val <- all_muscles_stabilized(timeseries_df, err)

  ForceTrialObj <- structure(rm_encoder_and_adept_cols(timeseries_df), adept_coordinates = adept_coordinates(timeseries_df),
    all_muscles_stabilized_by_last_val = all_muscles_stabilized_by_last_val)
  # Only compute stabilization info if the time series actually stabilized.
  if (all_muscles_stabilized_by_last_val) {
    setattr(ForceTrialObj, "stability_info", force_trial_to_stable_metrics(timeseries_df,
      last_n_milliseconds, muscle_of_interest))
    setattr(ForceTrialObj, "stability_df ", list_of_forces_to_stabilized_df(list(timeseries_df),
      full_df_path, err, full_df, muscle_of_interest))
  }

  class(ForceTrialObj) <- "ForceTrial"
  return(ForceTrialObj)
}
