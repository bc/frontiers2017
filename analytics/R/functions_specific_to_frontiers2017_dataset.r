# hardcoded specifically for
# realTimeData2017_08_16_13_23_42_subset_of_first_posture.csv
save_snapshot_for_first_posture <- function(raw_data_timeseries_df, output_filepath) {
  first_posture_data <- split_by_position(raw_data_timeseries_df$adept_x, raw_data_timeseries_df)[[1]]  #only get first posture
  forces <- split_by_reference_force(first_posture_data)[2:101]
  write.csv(first_posture_data, output_filepath, row.names = FALSE)
}
stabilization_err_99_percentile <- 0.4
muscle_names <- c("M0", "M1", "M2", "M3", "M4", "M5", "M6")
maximum_tendon_force <- 20
minimum_tendon_force <- 3
data_location <- "~/Resilio Sync/data/realTimeData2017_08_16_13_23_42.txt"

##' Add the Adept XY to the indices DF
##' @description
##' This function takes in the indices. the last final index is a 0, because it hasn't been included yet.
##' This function fills that index with the correct value for each of the lines sampled.
##' @param idxs list of two dataframes, that contain the coordinates of the points
##' @return idxs_with_adept added adept_x and adept_y columns
add_adept_xy_to_indices <- function(idxs, unique_postures) {
  new_idxs <- idxs
  new_idxs[[1]]$adept_x <- unique_postures$adept_x[1002:1208]
  new_idxs[[1]]$adept_y <- unique_postures$adept_y[1002:1208]
  new_idxs[[2]]$adept_x <- unique_postures$adept_x[2:1001]
  new_idxs[[2]]$adept_y <- unique_postures$adept_y[2:1001]
  return(new_idxs)
}

##' Get AdeptX and AdeptY numeric tuple from first row of dataframe
##' @param timeseries_df time series with the adept_x and adept_y columns
##' @return vector of adept_x,adept_y numeric values.
adept_coordinates <- function(timeseries_df) {
  df_head <- head(timeseries_df, 1)
  adept_x <- df_head$adept_x
  adept_y <- df_head$adept_y
  return(c(adept_x, adept_y))
}


##' Length of the force trial is within range of the acceptable length
##' @param force_time_series Dataframe of observations
##' @param desired_ms the ideal length (integer)
##' @param max_delta_acceptable integer The maximum aceptable residual from the desired_ms
##' @return is_acceptable logical result
force_ts_len_is_acceptable <- function(force_time_series, desired_ms = 800, max_delta_acceptable=50){
  observed_ms <- length(force_time_series[,1])
  if ((abs(observed_ms - desired_ms)) < max_delta_acceptable){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


##' Remove columns by a vector of colnames
##' @param timeseries_df dataframe with columns you want to remove
##' @param drops vector of string names of columns to rm
##' @return df_clean dataframe with dropped columns
##' TODO test
rm_cols <- function(timeseries_df, drops) {
  timeseries_df[, !(names(timeseries_df) %in% drops)]
}

##' Remove Encoder (angle_*) and Adept xy columns'
##' @param timeseries_df dataframe of the time series with all 39 columns
##' @return df dataframe without adept or angle columns
  rm_encoder_and_adept_cols <- function(timeseries_df){
    angle_column_names <- do.call("c", lapply(0:6, angle))
    drops <- c("adept_x", "adept_y", angle_column_names)
    return(rm_cols(timeseries_df, drops))
  }

##' Compose start and final indices into a clean little dataframe
##' @param line_postures_start_indices line_postures_start_indices
##' @return indices_df dataframe with the initial and final indices as the only elements
posture_indices_df <- function(line_posture_start_indices) {
  final <- c(line_posture_start_indices[-1] - 1, 0)
  initial <- c(line_posture_start_indices)
  return(data.frame(initial = initial, final = final))
}

##'Clean up the end of the postures
##' The last index of the postures needs to be added to indicate the end of data collection for each line.
##' Importantly, the second trial was interrupted, so we delete the last posture (as the forces did not complete)
##' I did a manual bisection of the indices to find the end of the 1000 posture line (element 1 of the input)
##' @param idxs_of_lines_1_and_2 list of two elements, each with the initial, final idx from full_df.
##' @return idxs_of_lines_1_and_2_fixed last elements are hard-coded based on the initial data.
clean_up_posture_indices <- function(idxs_of_lines_1_and_2) {
  idxs <- idxs_of_lines_1_and_2  #make copy
  idxs[[1]] <- idxs[[1]][-nrow(idxs[[1]]), ]
  idxs[[2]][1000, 2] <- 81369527
  return(idxs)
}

##' These are the encoder values, which aren't used for Frontiers2017
frontiers2017_columns_to_ignore <- c("angle_0", "angle_1", "angle_2", "angle_3",
  "angle_4", "angle_5", "angle_6")


##' These are the posture values, which are repeated across every data observation.
columns_to_extract_into_attributes <- function() {
  c(unlist(lapply(muscle_names, reference)), "adept_x", "adept_y")
}
