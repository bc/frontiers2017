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
forces <- get_forces_list(full_df, indices = idxs[[1]][1,1:2])

forces_per_posture <- lapply(df_to_list_of_rows(idxs[[1]]), function(row){
  get_forces_list(full_df, indices = c(row[['initial']], row[['final']]))
}

##' @param full_df dataframe including the column reference_M0 and row.names
##' @param indices where the posture starts and stops. 2 element vector of integers
##' @param column_to_separate_forces string, by default 'reference_M0'.
##' @return forces a list of time_series objects which contain ~800 observations, representing each force trial.
  get_forces_list <- function(full_df, indices, column_to_separate_forces = 'reference_M0'){
    posture_indices <- indices[1]:indices[2]
    # [-1] = remove the nullification force before we
    # changed to a new posture, and the intra-transition data
    forces <- split(full_df[posture_indices,], full_df[posture_indices,column_to_separate_forces])[-1]
    return(forces)
  }

  forces_for_posture_1 <- split_by_reference_force(
    full_df[initial[1]:(final[1]-1),])

  message("Splitting by force trials")
  list_of_postures <- split(full_df, list(full_df$adept_x, full_df$adept_y), drop = TRUE)

message("Plotting Settling Time Analysis")
  sample_settling <- data.frame(settling = runif(100, -20, 20), initial_tension = runif(100,
    3, 20), final_tension = runif(100, 3, 20))
  tension_settling_scatter(sample_settling)
}
