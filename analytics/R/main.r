options(error = NULL)
main <- function() {

  source("R/data_description.r")
  source("R/time_series_functions.r")
  source("R/functions_specific_to_frontiers2017_dataset.r")
  source("R/settling_time_analysis.r")

  message("Pulling full dataset. Expect completion in 2'")
  require(microbenchmark)
  data_load_mbm <- microbenchmark(full_df <- readRDS("~/Resilio Sync/data/realTimeData2017_08_16_13_23_42.rds"),
    times = 1)
  print(data_load_mbm)
  message("Identifying unique postures")
  unique_postures <- head(unique(full_df[c("adept_x", "adept_y")]), -1)
  message("Grouping postures by line")
  lines <- postures_grouped_by_line(unique_postures, x_fixed_val = -525, y_fixed_val = 68)
  message("Identifying indices for the start and end of each posture")

  line_posture_start_indices <- lapply(lines, function(line) as.numeric(rownames(line)))
  idxs <- add_adept_xy_to_indices(lapply(line_posture_start_indices, posture_indices_df), unique_postures)
  idxs_clean <- clean_up_posture_indices(idxs)
  message("Identifying forces within each posture")
  require(pbmcapply)
  line_1_rows_list <- df_to_list_of_rows(idxs_clean[[1]])
  browser()
  column_to_separate_forces <- reference("M0")
  a <- lapply(line_1_rows_list, function(index_row) {
    indices_tuple <- c(index_row[['initial']],index_row[['final']])
    browser()
    return(get_forces_list(full_df, indices_tuple, column_to_separate_forces))
  })


  apply_function_to_posture <- function(full_df, f, start_idx, end_idx){
    return(f(full_df[start_idx:(end_idx),]))
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
