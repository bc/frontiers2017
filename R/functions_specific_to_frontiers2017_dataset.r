# hardcoded specifically for
# realTimeData2017_08_16_13_23_42_subset_of_first_posture.csv
save_snapshot_for_first_posture <- function(raw_data_timeseries_df, output_filepath) {
  first_posture_data <- split_by_position(raw_data_timeseries_df$adept_x, raw_data_timeseries_df)[[1]]  #only get first posture
  forces <- split_by_reference_force(first_posture_data)[2:101]
  write.csv(first_posture_data, output_filepath, row.names = FALSE)
}
stabilization_err_99_percentile <- 0.4

##' Muscle Names from Frontiers2017 experiment
##' @return muscle_names list of strings of names, i.e. 'M0, ...'
muscle_names <- function() c("M0", "M1", "M2", "M3", "M4", "M5", "M6")


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

##' RDS paths to bound stability DFS
##' @param rds_postures list of full string paths to forcetrial rds objects
##' @return stability_df a dataframe cbind-ed from the stability info and stability df data.
rds_paths_to_bound_stability_dfs <- function(rds_postures) {
  message("Extracting StabilityDFs from RDS files")
  stability_df <- do.call("rbind", pbmclapply(rds_postures, function(rds_path) {
    ForceTrials_to_stability_df(readRDS(rds_path))
  }))
  message("Extracting StabilityInfo from RDS files")
  stability_infos <- do.call("rbind", pblapply(rds_postures, function(rds_path) {
    ForceTrials_to_stability_info_df(readRDS(rds_path))
  }))
  stability <- cbind(stability_df, stability_infos)
  return(stability)
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
force_ts_len_is_acceptable <- function(force_time_series, desired_ms = 800, max_delta_acceptable = 50) {
  observed_ms <- length(force_time_series[, 1])
  if ((abs(observed_ms - desired_ms)) < max_delta_acceptable) {
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
rm_encoder_and_adept_cols <- function(timeseries_df) {
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
  c(unlist(lapply(muscle_names(), reference)), "adept_x", "adept_y")
}

##' Split stability df into two postures (hardcoded for frontiers2017 data)
##' @param full_stability_df df with adept_x and adept_y column
##' @return df_list split dataframes
split_stability_df_into_postures <- function(full_stability_df) {
  fix_x_max_residual_and_sd <- full_stability_df[full_stability_df$adept_x == -525,
    ]
  fix_y_max_residual_and_sd <- full_stability_df[full_stability_df$adept_y == 68,
    ]
  return(list(fix_x = fix_x_max_residual_and_sd, fix_y = fix_y_max_residual_and_sd))
}



##' ensure_map_creation_ids_are_the_same
##' Stops if there are more than 1 unique value, or 0 map_creation_id values passed to it
##' @param map_creation_id_vector list of map_creation_ids as strings, or numbers.
ensure_map_creation_ids_are_the_same <- function(map_creation_id_vector) {
  map_vec_len <- length(unique(map_creation_id_vector))
  if (map_vec_len == 0) {
    stop("A length-0 vector of map_creation_ids were passed to ensure_map_creation_ids_are_the_same")
  } else if (length(unique(map_creation_id_vector)) > 1) {
    stop(paste("Expected only one type of map_creation_id, but instead got",
      map_vec_len))
  }
}
##' Split By Replicate
##' Takes a df of many replicates, and splits it up by significant time differences.
##' BEWARE it won't separate them unless they are at least 0.1 seconds apart from
##' one another. Will fail when mapID is repeated consecutively (please don't do this anyways)
##' TODO Rewrite so it will deal with consecutive values
##' TODO stop if replicate lengths are far from desired length
##' @param df_of_concatenated_replicates dataframe, witih column 'time'
##' @param approx_nrow expected number of samples for each of the replicates
##' @param tol tolerance for approx_nrow
##' @return list_of_replicates list of replicates, each a df with time column, etc. all will have same mapID
split_by_replicate <- function(df_of_concatenated_replicates, time_delta_threshold = 0.1) {
  delta_times <- diff(df_of_concatenated_replicates$time)
  final_indices_of_each_replicate <- which(delta_times > time_delta_threshold)
  initial_index <- 1
  counter <- 1
  res <- list()
  for (i in final_indices_of_each_replicate) {
    res[[counter]] <- df_of_concatenated_replicates[initial_index:i, ]
    counter <- counter + 1
    initial_index <- i + 1
  }
  res[[counter]] <- df_of_concatenated_replicates[i:nrow(df_of_concatenated_replicates),
    ]
  return(res)
}

##' column_sd_across_replicates
##' TODO test
##' @param list_of_trials list of dataframes, each with N colums
##' @param last_n_milliseconds number of milliseconds to base the stability metrics off.
column_sd_across_replicates <- function(list_of_trials, last_n_milliseconds) {
  apply(dcrb(lapply(lapply(list_of_trials, tail, 100), colMeans)), 2, sd)
}
##' column_mean_across_replicates
##' TODO test
##' @param list_of_trials list of dataframes, each with N colums
##' @param last_n_milliseconds number of milliseconds to base the stability metrics off.
list_of_trials_to_colMeans_of_last_n_milliseconds <- function(list_of_trials,last_n_milliseconds){
  lapply(lapply(list_of_trials, tail, last_n_milliseconds), colMeans)
}

##' lowest_l1_cost_soln'
##' @param df a dataframe where each row is a nrow(df)- dimensional vector
##' @param v a vector of numeric values
##' @return l1 numeric, the row with the lowest l1
lowest_l1_cost_soln <- function(df) df[which.min(rowSums(df)), ]

##' highest_l1_cost_soln'
##' @param df a dataframe where each row is a nrow(df)- dimensional vector
##' @param v a vector of numeric values
##' @return l1 numeric, the row with the highest l1
highest_l1_cost_soln <- function(df) df[which.max(rowSums(df)), ]

##' create_and_cbind_map_creation_ids
##' generates the maps'
##' TODO test
##' @param df_of_maps dataframe where each column is a muscle, and each row is a unique musle activation pattern (map)
##' @param muscles_of_interest vector of muscle name strings, i.e. 'M0', 'M1', ...
##' @return cbound dataframe with new inserted column, called 'map_creation_id'.
create_and_cbind_map_creation_ids <- function(df_of_maps, muscles_of_interest) {
  cbound <- cbind(generate_map_creation_ids(nrow(df_of_maps)), as.data.frame(df_of_maps))
  colnames(cbound) <- c("map_creation_id", muscles_of_interest)
  return(cbound)
}


##' df_of_hand_response_input_output
##' takes in the hand responses, outputs a dataframe that you can use to interpret
##' the static tension-force relationship.
##' result typically passed to df_split_into_training_and_testing
##' TODO Test or retire'
##' @param noise_hand_responses a list of multiple timeseries dataframes, each with the input muscle columns and output columns.
##' @return df input_output_data dataframe that concatenates each noise_hand_response into one row of the dataframe.
df_of_hand_response_input_output <- function(noise_hand_responses, last_n_milliseconds){
  dcrb(lapply(lapply(noise_hand_responses, tail, last_n_milliseconds), colMeans))
}
