last_n_milliseconds = 100


test_that("list of posture RDS paths to list of A matrices",{
  rds_folder_path <- "~/Resilio Sync/data/ForceTrials_at_each_posture/"
  rds_postures <- simplify2array(lapply(dir(rds_folder_path), prepend_string,rds_folder_path))
})

test_that("data for many postures can be used to create a list of A matrices",{
  rds_postures <- all_file_paths("~/Resilio Sync/data/ForceTrials_at_each_posture/")
  list_of_postures <- list_of_xy_to_df(pblapply(rds_postures, get_adept_coordinates_from_rds), c("adept_x","adept_y"))
  list_of_A_matrices <- list_of_posture_rds_files_to_list_of_A_matrices(rds_postures, last_n_milliseconds)
  vafs <- simplify2array(lapply(list_of_A_matrices, function(fit){
    variance_accounted_for(fit[[2]],fit[[3]])
  }))
  cb <- cbind(list_of_A_matrices, vafs)
  browser()
})


test_that("a_matrix <- forcetrial_list <- rds",{
  rds_folder_path <- "~/Resilio Sync/data/ForceTrials_at_each_posture/"
  sample_posture_path <- dir(rds_folder_path)[1]
  sample_posture_data <- readRDS(paste0(rds_folder_path, sample_posture_path))
  input_output_data <- converged_colmeans(sample_posture_data, last_n_milliseconds)
  A_1 <- find_A_matrix(input_output_data)
  expect_equal(length(A_1), 3)
})


test_that("an A matrix can be evaluated",{
})
