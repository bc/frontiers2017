context("test_data_description.r")
require(testthat)
source("../../R/data_description.r")
source("../../R/time_series_functions.r")
source("../../R/functions_specific_to_frontiers2017_dataset.r")
source("../../R/settling_time_analysis.r")

test_that("Show a sample snapshot of the data collected", {
  # first_data_chunk <- saveRDS(read.csv(data_location, nrows = 85000, header = TRUE), "first_data_chunk_85000_rows.rds")
  first_data_chunk <- read_rds_from_package_extdata("first_data_chunk_85000_rows.rds")
  output_filepath <-  "../../output/realTimeData2017_08_16_13_23_42_subset_of_first_posture.csv"
  # Save snapshot of just first posture
  save_snapshot_for_first_posture(first_data_chunk, output_filepath)

  message("Computing Data Description Analysis Figure")
  pdf("../../output/data_description_analysis.pdf", width = 14.2, height = 10)
  data_description_analysis(first_data_chunk, minimum_tendon_force, maximum_tendon_force,
    indices_of_interest = 2:5)
  dev.off()
})
