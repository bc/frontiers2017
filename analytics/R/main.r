#load all available material
source("R/time_series_functions.r")
source("R/data_description.r")
source("R/force_trial_stability.r")
source("R/functions_specific_to_frontiers2017_dataset.r")
source("R/generic_tools.r")
source("R/settling_time_analysis.r")

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
  message("Identifying unique postures. Expect competion in ")
  unique_postures <- head(unique(full_df[c("adept_x", "adept_y")]), -1)
  message("Grouping postures by line")
  postures_per_line <- postures_grouped_by_line(unique_postures, x_fixed_val = -525, y_fixed_val = 68)
  message("Identifying indices for the start and end of each posture")
  idx_dfs <- postures_to_idx_dfs(postures_per_line, unique_postures)

  message("Identifying forces within each posture")
  require(pbmcapply)
  line_1_rows_list <- df_to_list_of_rows(idx_dfs[[1]])
  column_to_separate_forces <- reference("M0")
  forces_at_fixed_x_postures <- pbmclapply(line_1_rows_list, function(index_row) {
    indices_tuple <- c(index_row[["initial"]], index_row[["final"]])
    list_of_forces <- get_forces_list(full_df, indices_tuple, column_to_separate_forces)
    return(list_of_forces)
  })
  browser()



  message("Splitting by force trials")
  list_of_postures <- split(full_df, list(full_df$adept_x, full_df$adept_y), drop = TRUE)
}
