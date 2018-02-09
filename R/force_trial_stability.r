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

##' Zero out JR3 sensors
##' @param df dataframe of raw timeseries data including JR3_FX, etc.
##' @param JR3_sensor_null vector of 6 values representing the mean in the first 100ms
zero_out_JR3_sensors <- function(df, JR3_sensor_null) {
  for (i in c("JR3_FX", "JR3_FY", "JR3_FZ", "JR3_MX", "JR3_MY", "JR3_MZ")) {
    df[,i] <- df[,i] - JR3_sensor_null[[i]]
  }
    return(df)
  }

##' coordinate frame translation from JR3 base to the fingertip
##' @param df dataframe of raw timeseries data including JR3_FX, etc.
##' @param distance distance in meters between base and the contact point.
##' @return df_translated dataframe with JR3 columns changed
  jr3_coordinate_transformation_along_z <- function(df, distance) {
    df$JR3_MX <- df$JR3_MX - distance * df$JR3_FY
    df$JR3_MY <- df$JR3_MY - distance * df$JR3_FX
    #MZ is unaffected by the tranlsation along the z axis.
    return(df)
    }


##' force_trial_to_stable_metrics
##' @param force_trial force trial time series
##' @param last_n_milliseconds the number of tail milliseconds from which we should calculate the settled standard deviation.
##' @param muscle the muscle of interest in a string e.g. 'M0'
##' @return vec_row dataframe row with reference, and max residual
force_trial_to_stable_metrics <- function(force_trial, last_n_milliseconds, muscle) {
  last_row <- tail(force_trial, 1)
  ref <- last_row[, reference(muscle)]
  measured_tail <- tail(force_trial, last_n_milliseconds)[, measured(muscle)]
  max_residual <- max(abs(range(measured_tail) - ref))
  vec_row <- data.frame(last_n_milliseconds = last_n_milliseconds, reference = ref,
    sd = sd(measured_tail), max_residual = max_residual)
  return(vec_row)
}

##' ForceTrial list to stability_df rows
##' @param list_of_force_trials each a force_trial time series
##' @param last_n_milliseconds the number of tail milliseconds from which we should calculate the settled standard deviation.
##' @param muscle the muscle of interest in a string e.g. 'M0'
##' @return stability_df_rows df with reference (desired) muscle tension, variance, maximum residual
ft_list_to_stability_df_rows <- function(list_of_force_trials, last_n_milliseconds,
  muscle) {
  reference_df_rows <- lapply(list_of_force_trials, force_trial_to_stable_metrics,
    last_n_milliseconds, muscle)
  return(do.call("rbind", reference_df_rows))
}

##' @title How many force trials meet the spec
##' @description if the spec is a certain envelope size for error?
##' Produces a plot of remaining fraction as a function of increasing err threshold. Draws a hardcoded line for the 99%
##' @param force_trials a list of raw force timeseries_DFs. has_settled has not been applied yet
##' @param err_lenout integer, the resolution along the X axis by which diff err thresholds will be evaluated
##' @importFrom parallel mclapply
try_different_stability_thresholds <- function(force_trials, err_lenout = 20, muscles_of_interest) {
  different_errors <- seq(0.1, 1, length.out = err_lenout)
  num_remaining_force_trials <- unlist(lapply(different_errors, function(err) {
    length(remove_unsettled_force_trials(force_trials, err, muscles_of_interest))
  }))
  plot_remaining_force_trial_fraction_as_function_of_err(different_errors, num_remaining_force_trials,
    length(force_trials))
  return(cbind(different_errors, num_remaining_force_trials))
}

##' Indices from a Posture DF row'
##'@param posture_df_row a dataframe with initial and final columns
##'@return indices a list of two elements, the initial and final index.
indices_from_posture_df_row <- function(posture_df_row) c(posture_df_row[["initial"]],
  posture_df_row[["final"]])

##' Compute and save ForceTrials to RDS files'
##' Turn a dataframe of collected observations, with square input to reference, into formal ForceTrial objects
##' Works with stabilized or nonstabilized force trials. It will add attributes of stabilization only for those that meet the err criteria
##' saves each to a folder
##' @param df_row dataframe with 1 row with columns including the initial and final indices in reference to a posture collected.
##' @param column_to_separate_forces i.e. 'measured_M0'
##' @param data_location path to realTimeData2017_08_16_13_23_42.txt
##' @param full_df realTimeData2017_08_16_13_23_42.rds
##' @param err max allowable residual from reference force for muscle of interest
save_ForceTrials_to_rds <- function(df_row, column_to_separate_forces, data_location,
  full_df, err) {
  indices <- indices_from_posture_df_row(df_row)
  force_trial_file_string_with_posture <- paste0("force_trial_adept_x_", df_row[["adept_x"]],
    "_adept_y_", df_row[["adept_y"]], ".rds")
  force_list <- get_forces_list(full_df, indices, column_to_separate_forces)
  print("Coerscing to ForceTrial")
  force_trials_list <- lapply(force_list, ForceTrial, data_location, full_df, err)
  print("Saving RDS to Resilio")
  save_rds_to_Resilio(force_trials_list, force_trial_file_string_with_posture)
  print("Cleaning up memory")
  rm(force_trials_list, force_trials_list)
}

##' Turn a dataframe of collected observations, with square input to reference, into formal ForceTrial objects
##' Works with stabilized or nonstabilized force trials. It will add attributes of stabilization only for those that meet the err criteria
##' saves each to a folder
##' @param df_of_postures dataframe with columns including the initial and final indices in reference to each posture group collected.
##' @param full_df dataframe object of the full dataset collected, from which the indices will extract the sections.
##' @importFrom pbmcapply pbmclapply
posture_start_finish_indices_to_L_of_ForceTrials <- function(df_of_postures, full_df,
  data_location, err) {
  lapply(df_to_list_of_rows(df_of_postures), save_ForceTrials_to_rds, column_to_separate_forces = "measured_M0",
    data_location, full_df, err)
}

##' Posture path to stability data frame
##' @param posturepath string full path to RDS
##' @return the stability df for that postures
posture_path_to_stability_df <- function(posturepath) {
  posture <- readRDS(posturepath)
  max_residuals_and_sd <- lapply(posture, ForceTrial_to_signed_max_residual_and_sd)
  stability <- cbind(ForceTrials_to_stability_info_df(posture), ForceTrials_to_stability_df(posture))
  adept_coords <- adept_coordinates_from_ForceTrial(posture[[1]])
  posture_and_residual_sd <- dcrb(lapply(max_residuals_and_sd, add_posture_to_max_residual_and_sd,
    adept_coords))
  d <- merge(stability, posture_and_residual_sd)
  return(d)
}
##' Get the signed residuals from all postures
##' @param rds_postures list of full filepaths to each rds Posture, each with a list of ForceTrials
##' @return stability_df data.frame with all stabiliyt information observations at postures. Includes adept xy coordinates
get_stability_df_for_all_postures <- function(rds_postures) {
  stability_df <- dcrb(pbmclapply(rds_postures, posture_path_to_stability_df))
  return(stability_df)
}
##' split_by_map_creation_id
##' TODO document and test.
##' @return force_dataframes a list of dataframes, each with a unique map_creation_id
split_by_map_creation_id <- function(unique_map_creation_ids, maps_data_df){
  force_dataframes <- lapply(unique_map_creation_ids, function(map_id){
    maps_data_df[maps_data_df$map_creation_id == map_id,]
  })
  return(force_dataframes)
}

##' Calibration Matrix for JR3 in basement lab Nov 2017
##' @return mat 6x6 calibration matrix to multiply by FX,FY, ... voltages
calibration_matrix <- function(){
  as.matrix(rbind(c(13.669,0.229,0.105,-0.272,0.060,0.865),
    c(0.160,13.237,-0.387,-0.027,-0.396,-0.477),
    c(1.084,0.605,27.092,-2.88,-0.106,1.165),
    c(-.007,-.003,-0.001,0.676,-0.002,-0.038),
    c(0.004,-0.004,0.001,0.000,0.688,-0.012),
    c(0.004,0.003,0.003,-0.006,0.013,0.665)))
  }

##' jr3_voltages_to_forces
##' @param dataframe_with_jr3_voltage_columns has JR3_FX, etc.
##' @return new_df  dataframe where voltages are now forces in either N or N*m for forces and torques, respectively.
jr3_voltages_to_forces <- function(dataframe_with_jr3_voltage_columns){
  just_jr3_cols <- dataframe_with_jr3_voltage_columns[,dots_to_underscores(force_column_names)]
  dataframe_with_jr3_voltage_columns[,dots_to_underscores(force_column_names)] <- t(calibration_matrix() %*% t(as.matrix(just_jr3_cols)))
  return(dataframe_with_jr3_voltage_columns)
}

##' Munge JR3 Data from raw voltages into bias-removed forces and torques.
##' Applies all relevant data cleaning processes to munge it into high quality format
##' 1. Zero out sensors from first 100ms of data
##' 1. Use calibration matrix provided by JR3 datasheet to convert from voltages to forces.
##' 2. Translate the coordinate frame from the JR3 surface to the fingertip location
##' 3. Remove all map_creation_id values that are 0.0 (used in setup and teardown)
##' @param raw_uncut_timeseries_data time series with the M0, M1, and JR3_FX, and time, etc.
##' @param input_are_voltages logical, by default FALSE. If you put in raw voltages then make this TRUE. It will use the calibration matrix to fix values to their N and Nm equivalents.
##' @param indices_for_null indices of the time series (row indices) that will be used to compute the 0'ing out of the JR3 sensor.
##' @param remove_nonzero_map_creation_ids logical, only put this in if you prescribed maps with the input CSV. if you were messing around while the motors were at rest, or the motors were off, just set this to false.
##' @param JR3_sensor_null the colMeans of the null section of a timeseries that is used to subtract/tare the bias from the JR3.
##' @return timeseries_data time series with the M0, M1, and JR3_FX, and time, etc, but with transformations applied.
munge_JR3_data <- function(raw_uncut_timeseries_data,JR3_to_fingertip_distance = 0.02, input_are_voltages=FALSE, indices_for_null=1:100, remove_nonzero_map_creation_ids = TRUE,JR3_sensor_null = NULL){
  if (is.null(JR3_sensor_null)){
    message('Computing JR3 null from user-provided indices_of_interest')
    JR3_sensor_null <- colMeans(raw_uncut_timeseries_data[indices_for_null,])
  }
  zeroed_uncut_timeseries_data <- zero_out_JR3_sensors(raw_uncut_timeseries_data, JR3_sensor_null)
  if(input_are_voltages){
    message("Converting voltages into forces and torques")
    zeroed_uncut_timeseries_data <- jr3_voltages_to_forces(zeroed_uncut_timeseries_data)
  }
  noise_response <- jr3_coordinate_transformation_along_z(zeroed_uncut_timeseries_data, JR3_to_fingertip_distance)
  # make sure the JR3 signals respond in some way to the changes.
  if (remove_nonzero_map_creation_ids){
    noise_response <- noise_response[noise_response$map_creation_id != 0, ]
  }
  return(noise_response)
}
##' length of element is within range (takes in list)
##' We only want the forcetrials that have the correct length to continue on for analysis.
##' However, sometimes forcetrials have 200 datapoints, instead of the desired 800. This function
##' is useful for quickly identifying which elements of a list of forcetrials land within the desired bounds of length.
##' It also outputs a message saying how many of the trials passed the criteria. This can be helpful when identifying throughput.
##' @param list_of_dataframes a list of elements that nrow can be applied to
##' @param bounds a 2 element vector of the lower and upper bounds that define the allowable lengths.
##' @return mask a vector of logical values, whether the corresponding index has a length that is within the desired range.
length_is_within_range <- function(list_of_dataframes, bounds){
  are_correct_length <- dcc(lapply(list_of_dataframes, function(dt) {
    return(nrow(dt) >= bounds[1] && nrow(dt) < bounds[2])
  }))
  return(are_correct_length)
}

##' split_by_map_and_remove_wrongly_lengthed_samples
##' used to split samples by map_creation_id, then remove samples that have a length that's too short or too long.
##' @param noise_response_wo_null a large dataframe of time seriers values, including JR3_FX, time, etc.
##' @param bounds 2 element vector that defines the allowable range for lengths of each of the forcetrials.
##' @return hand_responses A list of dataframes, each a timeseries with the columns of interest for muscles and forces.
split_by_map_and_remove_wrongly_lengthed_samples <- function(noise_response_wo_null,bounds=c(700,810)){
  noise_hand_responses_raw <- split_by_map_creation_id(unique(noise_response_wo_null$map_creation_id),
    noise_response_wo_null)
    are_correct_length <- length_is_within_range(noise_hand_responses_raw, bounds=bounds)
  noise_hand_responses <- noise_hand_responses_raw[are_correct_length]
  message(sprintf("Out of the %s collected maps, %s had between 700 and 810 samples. Using %s maps. \n Summary of raw lengths:",
    length(noise_hand_responses_raw), length(noise_hand_responses), length(noise_hand_responses)))
    print(summary(dcc(lapply(noise_hand_responses_raw, nrow))))
    return(noise_hand_responses)
}

##' A_fit_from_80_20_split & evaluate against test data.
##' at end also prints out results of fit_evaluation_without_offset(A_fit, as.data.frame(test_data))
##' @param input_output_data data including regressor and response. corresponding to muscles_of_interest and force_names_to_predict
##' @param muscles_of_interest,force_names_to_predict vector of strings. NOT reference muscles. must be "M0", etc.
##' @return A_fit fit object as a result from_A_matrix
A_fit_from_80_20_split <- function(input_output_data, muscles_of_interest, force_names_to_predict){
  data <- df_split_into_training_and_testing(input_output_data, fraction_training = 0.80)
  training_data <- data$train
  test_data <- data$test
  num_muscles <- length(muscles_of_interest)
  A_fit <- find_A_matrix_without_offset(as.data.frame(training_data), measured(muscles_of_interest),
    force_names_to_predict)
  fit_evaluation_without_offset(A_fit, as.data.frame(test_data))
  return(A_fit)
}
