#load all available material
source("R/time_series_functions.r")
source("R/data_description.r")
source("R/force_trial_stability.r")
source("R/ForceTrial.r")
source("R/linear_fit.r")
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



  message("Splitting by force trials")
  list_of_postures <- split(full_df, list(full_df$adept_x, full_df$adept_y), drop = TRUE)
}

##' Munge full_DF into the posture RDS files
produce_ForceTrial_rds_objects <- function(){
  posture_idxs_per_line <- read_rds_to_package_extdata("index_dataframes_for_two_posture_lines.rds") #hardcoded index dfs
  full_df <- readRDS("~/Resilio Sync/data/realTimeData2017_08_16_13_23_42.rds")
  print("Loading full_df. Expect 2'")
  err = 0.4
  last_n_milliseconds = 100
  fix_x_postures <- posture_idxs_per_line[[1]]
  fix_y_postures <- posture_idxs_per_line[[2]]
  many_postures_to_ForceTrials(posture_idxs_to_index_tuples(fix_x_postures), full_df, column_to_separate_forces = "reference_M0", err=0.4, last_n_milliseconds, save_rds=TRUE)
  many_postures_to_ForceTrials(posture_idxs_to_index_tuples(fix_y_postures), full_df, column_to_separate_forces = "reference_M0", err=0.4, last_n_milliseconds, save_rds=TRUE)
}
