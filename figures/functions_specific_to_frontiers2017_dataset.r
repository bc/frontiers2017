#hardcoded specifically for realTimeData2017_08_16_13_23_42_subset_of_first_posture.csv
save_snapshot_for_first_posture <- function(raw_data_timeseries_df, output_filepath="../data/realTimeData2017_08_16_13_23_42_subset_of_first_posture.csv") {
  first_posture_data <- split_by_position(first_data_chunk$adept_x, first_data_chunk)[[1]] #only get first posture
  forces <- split_by_reference_force(first_posture_data)[2:101]
  write.csv(first_posture_data, output_filepath, row.names=FALSE)
}
