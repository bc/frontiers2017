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

##' @description
##' this function takes in the indices. the last final index is a 0, because it hasn't been included yet.
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

posture_indices_df <- function(line_posture_start_indices){
  final <- c(line_posture_start_indices[-1]-1, 0)
  initial <- c(line_posture_start_indices)
  return(data.frame(initial = initial,final = final))
}

##'Clean up the end of the postures
##' The last index of the postures needs to be added to indicate the end of data collection for each line.
##' Importantly, the second trial was interrupted, so we delete the last posture (as the forces did not complete)
##' I did a manual bisection of the indices to find the end of the 1000 posture line (element 1 of the input)
##' @param idxs_of_lines_1_and_2 list of two elements, each with the initial, final idx from full_df.
##' @return idxs_of_lines_1_and_2_fixed last elements are hard-coded based on the initial data.
  clean_up_posture_indices <- function(idxs_of_lines_1_and_2){
    idxs <- idxs_of_lines_1_and_2 #make copy
    idxs[[1]] <- idxs[[1]][-nrow(idxs[[1]]),]
    idxs[[2]][1000,2] <- 81369527
  return(idxs)
}
