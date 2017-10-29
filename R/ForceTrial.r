##' Instantiate a Force Trial Structure Object
##' We chose M0 as the representative muscle to converge because from all
##' experiments it appeared to have a representative response for the other muscles
##' if any observations were recorded when the robot was initalized or moving, those rows were removed.
##' we remove the angle and adept columns because they are unused and redundant, respectively
##' @param adept_x Numeric position of adept (positive or negative)
##' @param adept_y Numeric position of adept (positive or negative)
##' @param timeseries_df dataframe of the time series with all 39 columns
##' @param err max allowable residual from desired reference force.
##' @param last_n_milliseconds number of milliseconds to base the stability metrics off.
##' @return forcetrial_structure_object the object with all relevant attributes and stability metrics
ForceTrial <- function(timeseries_df, full_df, err, last_n_milliseconds = 100,
  muscle_of_interest = "M0") {
  if (1 %in% timeseries_df$robot_flag) {
    len_before <- nrow(timeseries_df)
    timeseries_df <- timeseries_df[timeseries_df$robot_flag == 2, ]
    num_trimmed <- len_before - nrow(timeseries_df)
    warnings(paste("robot was moving during force trial. Trimmed", num_trimmed,
      "indices."))
  }
  if (0 %in% timeseries_df$robot_flag) {
    warnings("robot was not initialized correctly during force trial")
  }
  all_muscles_stabilized_by_last_val <- all_muscles_stabilized(timeseries_df, err)
  ForceTrialObj <- structure(rm_encoder_and_adept_cols(timeseries_df), adept_coordinates = adept_coordinates(timeseries_df),
    all_muscles_stabilized_by_last_val = all_muscles_stabilized_by_last_val)
  # Only compute stabilization info if the time series actually stabilized.
  if (all_muscles_stabilized_by_last_val) {
    setattr(ForceTrialObj, "stability_info", force_trial_to_stable_metrics(timeseries_df,
      last_n_milliseconds, muscle_of_interest))
    setattr(ForceTrialObj, "stability_df ", list_of_forces_to_stabilized_df(list(timeseries_df), err, full_df, muscle_of_interest))
  }
  class(ForceTrialObj) <- "ForceTrial"
  return(ForceTrialObj)
}

##' Extract Adept Coordinates from a Force Trial object
##' @param ForceTrial object of type ForceTrial
##' @return adept_coordinates a tuple vector of c(adept_x, adept_y)
adept_coordinates_from_ForceTrial <- function(ForceTrial) {
  return(attr(ForceTrial, "adept_coordinates"))
}

##' Get mask of forceTrials which stabilized
##' @param ForceTrial_list a list of ForceTrial objects
##' @return stablized_mask vector of logicals
ForceTrials_which_stabilized <- function(ForceTrial_list) {
  simplify2array(lapply(ForceTrial_list, ForceTrial_stabilized))
}

##' Evaluate whether a ForceTrial stabilized'
##' @param ft object of type ForceTrial
##' @return logical, whether all uscles stabilized by the last val
ForceTrial_stabilized <- function(ft) {
  attr(ft, "all_muscles_stabilized_by_last_val")
}

##' Extract observations DF from the ForceTrial
##' @param ForceTrial object of type ForceTrial
##' @param ForceTrial_df dataframe of the raw observations
get_df_from_ForceTrial <- function(ForceTrial) {
  data.frame(ForceTrial[names(ForceTrial)])
}

##' list of force trials to converged colmeans
##' @param ForceTrial_list a list of ForceTrial objects
##' @param last_n_milliseconds the number of tail milliseconds from which we should calculate the settled standard deviation.
converged_colmeans <- function(ForceTrial_list, last_n_milliseconds) {
  as.data.frame(do.call("rbind",
  lapply(
    lapply(
      lapply(ForceTrial_list, get_df_from_ForceTrial),
      tail,
      last_n_milliseconds),
      colMeans)
    )
  )
}

##' Coerce ForceTrial into a ForceTrial dataframe
##' @param ForceTrial object of type ForceTrial
##' @return df raw dataframe of the force trial.
ft_to_df <- function(ForceTrial) {
  return(as.data.frame(do.call("cbind", ForceTrial)))
}


##' Get a stability df from a list of Force Trials
##' @param list_of_force_trials List of force trials
##' @return stability_df a stability data frame with initial_reference_force delta_force amortized_velocity_of_force, initial_index final_index final_reference_force settling_time
ForceTrials_to_stability_df <- function(list_of_force_trials) {
  extract_and_rbind_attribute(list_of_force_trials, attribute_to_extract = "stability_df")
}

##' Get a stability_info df from a list of Force Trials
##' @param list_of_force_trials List of force trials
##' @return stability_info_df a stability_info data frame with last_n_milliseconds reference sd max_residual
ForceTrials_to_stability_info_df <- function(list_of_force_trials) {
  extract_and_rbind_attribute(list_of_force_trials, attribute_to_extract = "stability_info")
}

##' Extract a row-like attribute from a list of elements
##' rbind them into a large dataframe
##' @param list_of_elements list of elements
##' @param attribute_to_extract string, of the element that will be called from each element via attr()
##' @return attribute_df a data_frame of the combined attributes across all elements of the list.
extract_and_rbind_attribute <- function(list_of_elements, attribute_to_extract) {
  rbind_dfs(lapply(list_of_elements, function(x) {
    attr(x, attribute_to_extract)
  }))
}


##' ForceTrial_to_signed_max_residual'
##' @param force_trial element of type ForceTrial
##' @return max_residual_val single numeric signed value
ForceTrial_to_signed_max_residual <- function(force_trial) {
  df_to_max_signed_residual(ft_to_df(force_trial))
}

##' ForceTrial_to_max_residual_and_sd'
##' @param force_trial element of type ForceTrial
##' @return max_residual_val_sd tuple numeric values
ForceTrial_to_signed_max_residual_and_sd <- function(force_trial) {
  df <- ft_to_df(force_trial)
  return(data.frame(signed_max_residual = df_to_max_signed_residual(df), sd = df_to_sd(df)))
}


##' df_to_max_signed_residual
##' @param ft force trial dataframe
##' @return max_residual_signed the maximal residual from desired force. signed as positive or negative.
df_to_max_signed_residual <- function(ft) {
  a <- tail(ft$measured_M0, 100) - tail(ft$reference_M0, 100)
  max_residual <- signed_max_residual_val(range(a))
  return(max_residual)
}


##' df_to_sd
##' @param ft force trial dataframe
##' @return sd numeric value of sd in last 100 ms
df_to_sd <- function(ft) {
  last_100_vals <- tail(ft$measured_M0, 100)
  standard_deviation <- sd(last_100_vals)
  return(standard_deviation)
}
