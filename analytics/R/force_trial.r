##' Instantiate a Force Trial Structure Object
##' We chose M0 as the representative muscle to converge because from all
##' experiments it appeared to have a representative response for the other muscles
##' @param adept_x Numeric position of adept (positive or negative)
##' @param adept_y Numeric position of adept (positive or negative)
##' @param timeseries_df dataframe of the time series with all 39 columns
##' @param last_n_milliseconds number of milliseconds to base the stability metrics off.
##' @return forcetrial_structure_object the object with all relevant attributes and stability metrics
forcetrial_structure_object <- function(timeseries_df, full_df_path, full_df, err,
  last_n_milliseconds = 100, muscle_of_interest = "M0") {
  stability_df <- list_of_forces_to_stabilized_df(list(timeseries_df), full_df_path,
    err, full_df, muscle_of_interest)
  stability_info <- force_trial_to_stable_metrics(timeseries_df, last_n_milliseconds,
    muscle_of_interest)
  adept_x <- head(timeseries_df, 1)$adept_x
  adept_y <- head(timeseries_df, 1)$adept_y
  # remove the encoder and adept data columns that aren't being used for this analysis.
  angle_column_names <- do.call("c", lapply(0:6, angle))
  drops <- c("adept_x", "adept_y", angle_column_names)
  rm_cols(timeseries_df, drops)

  forcetrial_structure_object <- structure(df, adept_coordinates = c(adept_x, adept_y),
    stability_info = stability_info, stability_df = stability_df)
  return(forcetrial_structure_object)
}