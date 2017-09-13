options(error = NULL)
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

  message("Pulling full dataset. Expect completion in 2'")
  require(microbenchmark)
  data_load_mbm <- microbenchmark(full_df <- readRDS("~/Resilio Sync/data/realTimeData2017_08_16_13_23_42.rds"),
    times = 1)
  print(data_load_mbm)
  message("Identifying unique postures")
  unique_postures <- head(unique(full_df[c("adept_x", "adept_y")]), -1)
  message("...")
  lines <- postures_grouped_by_line(unique_postures, x_fixed_val = -525, y_fixed_val = 68)
  message('line_posture_start_indices')
  line_posture_start_indices <- lapply(lines, function(line) as.numeric(rownames(line)))
  message('...')
  idxs <- fix_last_posture_of_index_dfs(add_adept_xy_to_indices(lapply(line_posture_start_indices, posture_indices_df), unique_postures))
  forces_per_posture_fixed_y <- forces_per_posture(idxs[[2]], full_df)
  force_trials_per_posture <- lapply(forces_per_posture_fixed_y, rm_points_where_adept_robot_is_moving)
  force_trials_per_posture[-1] #remove starting posture at adept_x = 0, adept_y = 0
  browser()

message("Plotting Settling Time Analysis")
  sample_settling <- data.frame(settling = runif(100, -20, 20), initial_tension = runif(100,
    3, 20), final_tension = runif(100, 3, 20))
  tension_settling_scatter(sample_settling)
}
