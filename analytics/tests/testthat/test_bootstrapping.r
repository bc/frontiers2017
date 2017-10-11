test_that("you can produce an A matrix with fewer than all datapoints provided", {
  rds_postures <- all_file_paths("~/Resilio Sync/data/ForceTrials_at_each_posture/")
  list_of_postures <- list_of_xy_to_df(pblapply(rds_postures, get_adept_coordinates_from_rds), c("adept_x","adept_y"))
  list_of_A_matrices <- list_of_posture_rds_files_to_list_of_A_matrices(rds_postures, last_n_milliseconds)
})
