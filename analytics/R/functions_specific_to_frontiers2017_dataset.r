# hardcoded specifically for
# realTimeData2017_08_16_13_23_42_subset_of_first_posture.csv
save_snapshot_for_first_posture <- function(raw_data_timeseries_df, output_filepath = "../output/realTimeData2017_08_16_13_23_42_subset_of_first_posture.csv") {
  first_posture_data <- split_by_position(raw_data_timeseries_df$adept_x, raw_data_timeseries_df)[[1]]  #only get first posture
  forces <- split_by_reference_force(first_posture_data)[2:101]
  write.csv(first_posture_data, output_filepath, row.names = FALSE)
}

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

posture_indices_df <- function(line_posture_start_indices) {
  final <- c(line_posture_start_indices[-1] - 1, 0)
  initial <- c(line_posture_start_indices)
  return(data.frame(initial = initial, final = final))
}

##' 81520 is the length of the last posture where y was fixed, via manual id by cohn
##' @param idxs dataframe, that contain the coordinates of the points, and adept_x and adept_y columns
##' @return fixed_posture_indices now with rm posture 208, and added final point for last 1000th fixed-y posture.
fix_last_posture_of_index_dfs <- function(idx_df) {
  fix_x_postures <- head(idxs[[1]], -1)  #the last one was cut off
  last_idx_where_y_fixed <- 81288007 + 81520
  idxs[[2]][1000, "final"] <- last_idx_where_y_fixed
  fix_y_postures <- idxs[[2]]
  fixed_posture_indices <- list(fix_x_postures, fix_y_postures)
  return(fixed_posture_indices)
}
