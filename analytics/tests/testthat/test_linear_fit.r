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
  cb <- data.frame(cbind(list_of_postures, vafs))
  expect_equal(nrow(cb),1206)
  fix_x_vaf <- cb[cb$adept_x == -525.000,]
  fix_y_vaf <- cb[cb$adept_y == 68.000,]
  expect_equal(nrow(fix_x_vaf),206)
  expect_equal(nrow(fix_y_vaf),1000)
  ##Plot figure
    p1 <- posture_dependency_plot(fix_y_vaf, "adept_x", "vafs")
    p2 <- posture_dependency_plot(fix_y_vaf, "adept_x", "vafs")
    require(gridExtra)
    final <- gridExtra::grid.arrange(p1,p2, ncol = 2)
    ggsave("posture_dependency_adept_xy.pdf", final, width = 7, height = 4, dpi=600)
    ## end plot figure
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

rds_posture_to_bootstrap_analysis <- function(rds_posture_string_path, sample_size){
}

test_that("an A matrix can be evaluated",{
})
