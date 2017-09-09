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
