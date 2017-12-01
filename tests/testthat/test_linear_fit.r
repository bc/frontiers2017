context('test_linear_fit.r')
last_n_milliseconds = 100

test_that("a_matrix with fewer than all samples trained on <- forcetrial_list <- rds",
  {
    rds_folder_path <- "~/Resilio Sync/data/ForceTrials_at_each_posture/"
    sample_posture_path <- dir(rds_folder_path)[15]
    sample_posture_data <- readRDS(paste0(rds_folder_path, sample_posture_path))
    input_output_data <- converged_colmeans(sample_posture_data, last_n_milliseconds = 100)

    linear_model <- generate_linear_static_model(input_output_data, fraction_training = 0.8)
    print(paste(median(linear_model$euclidian_errors), "is the median euclidian err"))
    lm_measured <- lm(cbind(JR3.FX + JR3.FY + JR3.FZ) ~ measured_M0+measured_M1+measured_M2+measured_M3+measured_M4+measured_M5+measured_M6, data = input_output_data)
    cvlm <- cv.lm(input_output_data, lm_measured, m=10) # 3 fold cross-validation

    train_test <- df_split_into_training_and_testing(input_output_data, 0.8)
    trained_model <- lm(formula = cbind(JR3.FX, JR3.FY, JR3.FZ) ~ measured_M0 +
      measured_M1 + measured_M2 + measured_M3 + measured_M4 + measured_M5 +
      measured_M6, data = train_test$test, model = TRUE, x = TRUE, y = TRUE,
      qr = TRUE)
    test_results <- predict.lm(trained_model, train_test$test[, do.call("c",
      lapply(muscle_names(), measured))])

    input_output_data_0_mean <- apply(input_output_data, 1, function(row) row -
      apply(input_output_data, 2, mean))


    linear_model <- generate_linear_static_model(input_output_data_0_mean, fraction_training = 0.8)
    print(paste(median(linear_model$euclidian_errors), "is the median euclidian err"))
    hist(linear_model$euclidian_errors)


    tensions_and_forces_colnames <- c(do.call("c", lapply(muscle_names(), measured)),
      force_column_names)
    expect_true(implemented <- FALSE)  #TODO
  })

test_that('we can apply a nn to mapping',{
  rds_folder_path <- "~/Resilio Sync/data/ForceTrials_at_each_posture/"
  sample_posture_path <- dir(rds_folder_path)[15]
  sample_posture_data <- readRDS(paste0(rds_folder_path, sample_posture_path))
  input_output_data <- converged_colmeans(sample_posture_data, last_n_milliseconds = 100)

  nn <- neuralnet(
  JR3.FX + JR3.FY + JR3.FZ ~ measured_M0 + measured_M1 + measured_M2 + measured_M3 + measured_M4 + measured_M5 + measured_M6,
  data=input_output_data, hidden=c(6,6,6,6,6), err.fct="sse",
  linear.output=FALSE)
  plot(nn)
})

test_that("we can extract posture RDS files, and compute an RDS with the stabilized mapping for use with training",
  {
    rds_folder_path <- "~/Resilio Sync/data/ForceTrials_at_each_posture/"
    list_of_input_output_data <- pbmclapply(dir(rds_folder_path), function(rdspath) {
      posture <- readRDS(paste0(rds_folder_path, rdspath))
      adept_coordinates <- adept_coordinates_from_ForceTrial(posture[[1]])
      input_output_data <- converged_colmeans(posture, last_n_milliseconds = 100)
      attr(input_output_data, "adept_coordinates") <- adept_coordinates
      return(input_output_data)
    })

    saveRDS(list_of_input_output_data, "list_of_input_output_data.rds")
  })

test_that("we can visualize the output forces in different groups", {
  list_of_input_output_data <- readRDS("list_of_input_output_data.rds")
  firstposture <- list_of_input_output_data[[1]]
  all_fts <- dcrb(list_of_input_output_data)

  x_is_fixed <- dcc(lapply(list_of_input_output_data, function(x) {
    attr(x, "adept_coordinates")[1] == -525
  }))

  adept_x_coords <- dcc(lapply(list_of_input_output_data, function(x) attr(x, "adept_coordinates")[1]))
  adept_y_coords <- dcc(lapply(list_of_input_output_data, function(x) attr(x, "adept_coordinates")[2]))
  along_y_line <- dcrb(list_of_input_output_data[which(x_is_fixed)])
  along_x_line <- dcrb(list_of_input_output_data[which(!x_is_fixed)])

  expect_true(sum(attr(firstposture, "adept_coordinates")) - sum(c(-515.0074, 68)) <
    0.001)

  roygbiv <- c("red", "orange", "yellow", "green", "blue", "violet")
  rgl.open()
  rgl.bg(color = "white")
  plot3d(along_y_line$JR3.FX, along_y_line$JR3.FY, along_y_line$JR3.FZ, sub = "along_y_line of postures",
    aspect = TRUE)
  rgl.open()
  rgl.bg(color = "white")
  plot3d(along_x_line$JR3.FX, along_x_line$JR3.FY, along_x_line$JR3.FZ, sub = "along_x_line of postures",
    aspect = TRUE)
})



test_that("data for many postures can be used to create a list of A matrices", {
  rds_postures <- all_file_paths("~/Resilio Sync/data/ForceTrials_at_each_posture/")
  list_of_postures <- list_of_xy_to_df(pbmclapply(rds_postures, get_adept_coordinates_from_rds),
    c("adept_x", "adept_y"))
  list_of_A_matrices <- posture_rds_files_to_list_of_A_matrix_fits(rds_postures,
    last_n_milliseconds)
  vafs <- simplify2array(lapply(list_of_A_matrices, function(fit) {
    variance_accounted_for(fit[[2]], fit[[3]])
  }))
  cb <- data.frame(cbind(list_of_postures, vafs))
  expect_equal(nrow(cb), 1206)
  fix_x_vaf <- cb[cb$adept_x == -525, ]
  fix_y_vaf <- cb[cb$adept_y == 68, ]
  expect_equal(nrow(fix_x_vaf), 206)
  expect_equal(nrow(fix_y_vaf), 1000)
  # Plot figure
  fix_y <- posture_dependency_plot(fix_y_vaf, "adept_x", "vafs")
  fix_x <- posture_dependency_plot(fix_x_vaf, "adept_y", "vafs")
  require(gridExtra)
  final <- gridExtra::grid.arrange(fix_y, fix_x, ncol = 2)
  ggsave("../../output/posture_dependency_adept_xy.pdf", final, width = 14, height = 8,
    dpi = 600)
})

test_that("we can explore many different sample sizes for train/test splits", {
  sample_sizes <- 1:100
  model_performance <- 1:100  #TODO
  plot(sample_sizes, model_performance)
  expect_true(implemented <- FALSE)  #TODO
})


test_that("list of posture RDS paths to list of A matrices", {
  rds_folder_path <- "~/Resilio Sync/data/ForceTrials_at_each_posture/"
  rds_postures <- simplify2array(lapply(dir(rds_folder_path), prepend_string, rds_folder_path))
})



test_that("a_matrix <- forcetrial_list <- rds", {
  rds_folder_path <- "~/Resilio Sync/data/ForceTrials_at_each_posture/"
  sample_posture_path <- dir(rds_folder_path)[1]
  sample_posture_data <- readRDS(paste0(rds_folder_path, sample_posture_path))
  input_output_data <- converged_colmeans(sample_posture_data, last_n_milliseconds)
  A_1 <- find_A_matrix(input_output_data)
  expect_equal(length(A_1), 3)
})

rds_posture_to_bootstrap_analysis <- function(rds_posture_string_path, sample_size) {
}
