options(error = recover)
main <- function() {

  # make sure you run main.r from the figures directory for saving to work
  # setwd('~/Documents/GitHub/bc/frontiers2017/figures') Make sure you have already
  # installed the dependencies: install.packages(c('ggplot2', 'caTools',
  # 'testthat', 'GGally', 'plotrix','scatterplot3d'))
  source("R/data_description.r")
  source("R/time_series_functions.r")
  source("R/functions_specific_to_frontiers2017_dataset.r")
  source("R/settling_time_analysis.r")

  # Fig DataDescription Only take first part of data to create data description
  # figure. (60e3 = first 60 seconds)
  first_data_chunk <- read.csv(data_location, nrows = 85000, header = TRUE)

  # Save snapshot of just first posture
  save_snapshot_for_first_posture(raw_data_timeseries_df = first_data_chunk)

  message("Computing Data Description Analysis Figure")
  pdf("../output/data_description_analysis.pdf", width = 14.2, height = 10)
  data_description_analysis(first_data_chunk, minimum_tendon_force, maximum_tendon_force,
    indices_of_interest = 2:5)
  dev.off()

  # settling_time_analysis(data_location)
  message("Pulling full dataset. Expect completion in 2'")
  require(microbenchmark)
  data_load_mbm <- microbenchmark(full_df <- readRDS("~/Resilio Sync/data/realTimeData2017_08_16_13_23_42.rds"),
    times = 1)
  print(data_load_mbm)

  # unique() pulls the row names from the first occurrences of the unique rows. RM
  # last row of NA values
  message("Identifying unique postures")
  unique_postures <- head(unique(full_df[c("adept_x", "adept_y")]), -1)
  lines <- postures_grouped_by_line(unique_postures, x_fixed_val = -525.0000, y_fixed_val = 68.0000)
  browser()
  line_settling_dfs <- lapply(lines, get_settling_time_df)
  message("Splitting by force trials")
  list_of_postures <- split(full_df, list(full_df$adept_x, full_df$adept_y), drop = TRUE)
}


message("Computing Settling Time Analysis")
sample_settling <- data.frame(settling = runif(100, -20, 20), initial_tension = runif(100,
  3, 20), final_tension = runif(100, 3, 20))

tension_settling_scatter(sample_settling)


# get_settling_time_df <- function(list_of_unique_postures, full_df){
#   mclapply(list_of_unique_postures)
# }

main()
