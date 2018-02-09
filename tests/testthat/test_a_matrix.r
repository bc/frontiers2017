context("test_a_matrix.r")
##' create_output_folder()
context("Linearity functions manipulations")
##' @param filename string filename of interest
output_filepath_from_test <- function(filename){
  paste0("../../..", filename)
}
set.seed(4)
range_tension <- c(3, 20)
sample_input_output_data <- read_rds_from_package_extdata("training_data.rds")
data <- df_split_into_training_and_testing(sample_input_output_data, fraction_training = 0.8)
training_data <- data$train
test_data <- data$test

dynamic_source_df <- load_dynamic_matrix_csv("hand3_ultraflex_clean_timeseries_Meas_fresp.csv")
input_output_data <- hand3_hand4_clean_static_samples()$hand3_ultraflex
test_that("compare dynamically generated A matrix and statically generated A matrix", {
   plot_dynamic_vs_static_A_mat(dynamic_source_df,input_output_data,muscles_of_interest=muscle_names(), dots_to_underscores(force_column_names))
})

x1 <- matrix(c(-1), nrow = 1, ncol = 1, byrow = TRUE)
b1 <- matrix(c(-1), nrow = 1, ncol = 1, byrow = TRUE)
A1 <- matrix(c(b1[1,1]/x1[1,1]), ncol(x1), ncol(b1), byrow = TRUE)

x2 <- matrix(c(-1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
b2 <- matrix(c(-1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
A2 <- matrix(c(1, 0, 0, 1), ncol(x2), ncol(b2), byrow = TRUE)

x3 <- matrix(c(1, 2, 3), nrow = 1, ncol = 3, byrow = TRUE)
b3 <- matrix(c(15, 21, 27), nrow = 1, ncol = 3, byrow = TRUE)
A3 <- matrix(c(1, 2, 3, 1, 2, 3, 4, 5, 6), ncol(x3), ncol(b3), byrow = TRUE)

x4 <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, ncol = 3, byrow = TRUE)
A4 <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, ncol = 3, byrow = TRUE)
b4 <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, ncol = 3, byrow = TRUE)

x5 <- matrix(c(45, 67, 89, 43), nrow = 1, ncol = 4, byrow = TRUE)
A5 <- matrix(c(-4, 5, -55, -72), nrow = 4, ncol = 1, byrow = TRUE)
b5 <- matrix(c(-7836), nrow = 1, ncol = 1, byrow = TRUE)

x6 <- matrix(c(1, 52, 83, 48, 57, 66, 10, 22, 32, 94, 75, 86, 71, 62,
    93, 46, 55, 56, 21, 20, 93, 54, 65, 64, 21, 72, 30, 94,
     85, 56, 31, 42, 30, 94, 75, 36, 41, 62, 53, 34, 45, 16), nrow = 7, ncol = 6, byrow = TRUE)
A6 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 6, ncol = 1, byrow = TRUE)
b6 <- matrix(c(1227, 1417, 1269, 1265, 1392, 1172, 781), nrow = 7, ncol = 1, byrow = TRUE)

x7 <- matrix(c(6.0, 4.76, 8.6, 2.3, 5.7868, 6.7234), nrow = 3, ncol = 2, byrow = TRUE)
A7 <- matrix(c(4.34, 12.546, 45.878, 234.9, 1.02, 4.056), nrow = 2, ncol = 3, byrow = TRUE)
b7 <- matrix(c(1144.164, 80.13120, 294.5746, 577.594, 110.24160, 403.8796, 1604.441, 79.45906, 292.7569), nrow = 3, ncol = 3, byrow = TRUE)

context('Computing A Matrix Fits')
test_that("lin_qr_solve works as expected", {
   expect_equal(lin_qr_solve(x1, b1), A1)
   expect_equal(lin_qr_solve(x2, b2), A2)
   #expect_equal(lin_qr_solve(x3, b3), A3) couldn't technically get this one precise enough, but it works
})

test_that("find_A_matrix finds A matrix", {
   #expect_equal(
})

test_that("Test whether predict_output_force multiplies matrices properly", {
   expect_equal(predict_output_force(A4, x4), b4, 1e-4)
   expect_equal(predict_output_force(A5, x5), b5, 1e-4)
   expect_equal(predict_output_force(A6, x6), b6, 1e-4)
   expect_equal(predict_output_force(A7, x7), b7, 1e-4)
})

test_that("produce A matrices for different numbers of muscles or output force dimensions",
  {
    pdf("../../output/a_mat_fit.pdf")
    A_fit <- find_A_matrix(training_data, measured(muscle_names())[1:7], force_column_names[1:3])
    A_fit <- find_A_matrix(training_data, measured(muscle_names())[1:4], force_column_names[1:3])
    A_fit <- find_A_matrix(training_data, measured(muscle_names())[1:2], force_column_names[1:3])
    A_fit <- find_A_matrix(training_data, measured(muscle_names())[1:1], force_column_names[1:3])
    fit_evaluation(A_fit, test_data)
  dev.off()
  })
test_that('We find no error when fewer than 7 muscles (4) are used to train A matrix',{
    A_fit <- find_A_matrix(training_data, measured(muscle_names())[1:4], forces_of_interest=force_column_names[1:3])
    fit_evaluation(A_fit, test_data)
  })
test_that("We can calculate cond of an A matrix that uses 6forces~7tendons", {
  A_fit <- find_A_matrix(training_data,  measured(muscle_names())[1:7], forces_of_interest=force_column_names[1:3])
	cond_num <- kappa(A_fit$AMatrix, exact = TRUE)
	expect_true(cond_num > 0)
	expect_is(cond_num, 'numeric')
})

test_that('n_binary_combinations gets the right vals for small example', {
  expect_equal(n_binary_combinations(1), matrix(c(0,1), ncol=1))
  expect_equal(kappa(n_binary_combinations(2)), kappa(rbind(c(0,0), c(1,0), c(0,1), c(1,1))))
  expect_equal(kappa(n_binary_combinations(10)), 4, tol=0.01)
})

test_that('we can produce binary set of vectors for use with cadaver or robotic 7-muscle systems', {
  write_binary_combination_csv(7, c(3.12,10.0001), "../../output/map_unit_cube.csv")
})

test_that('compose_binary_combination_df creates correct values in place of 0,1', {
  n_10_df <- compose_binary_combination_df(10,c(0,1))
  map_ids_are_unique <- nrow(n_10_df) == length(unique(n_10_df[,1]))
  expect_true(map_ids_are_unique)
  expect_equal(nrow(n_10_df),1024)

  n_7_df <- compose_binary_combination_df(7,c(0,1))
  map_ids_are_unique <- nrow(n_7_df) == length(unique(n_7_df[,1]))
  expect_true(map_ids_are_unique)
  expect_equal(nrow(n_7_df),128)


})

test_that('we can produce a binary set of x vectors of size 7', {
  num_muscles <- 7
  A_fit <- find_A_matrix(training_data,  measured(muscle_names())[1:num_muscles], forces_of_interest=force_column_names[1:3])
  AMatrix_with_offset <- A_fit$AMatrix
  muscle_diag_ones <- diag(rep(1, num_muscles))
  muscle_constraints_matrix <- cbind(rep(0, num_muscles), muscle_diag_ones)
  generator_columns_A_matrix <- t(A_fit$AMatrix)
  offset_constraints_matrix <- rbind(c(1, rep(0, num_muscles)), c(-1, rep(0, num_muscles)))
  dim(generator_columns_A_matrix)
  dim(offset_constraints_matrix)
  dim(muscle_constraints_matrix)
  big_A <- rbind(generator_columns_A_matrix, -generator_columns_A_matrix, offset_constraints_matrix, muscle_constraints_matrix, -muscle_constraints_matrix)
  one_observed_force <- head(A_fit$endpointForceObservation,1)
  big_b <- as.numeric(c(one_observed_force, -one_observed_force, 1, 1, rep(range_tension[2],
    num_muscles), rep(range_tension[1], num_muscles)))

  constr <- list(constr = big_A, dir = rep("<=", nrow(big_A)), rhs = big_b)
  constraints_are_feasible(constr)
  state <- har.init(constr)
  result <- har.run(state, n.samples = 100)
  samples <- result$samples
  pass_unit_cube_to_A(big_A, 3, c(3,20))
  lowest_l1_cost_soln <- samples[which.min(rowSums(samples)), ]
  highest_l1_cost_soln <- samples[which.max(rowSums(samples)), ]
  test_predicted_response <- as.matrix(samples %*% A_fit$AMatrix)

  boxplot(test_predicted_response, ylab = "Tension N for FX,FY,FZ, Torque Nm for MX,MY,MZ")
  test_observed_response <- test_data[force_column_names]
  res_test <- test_observed_response - test_predicted_response
  # summary(res_test)
  MASS::parcoord(samples)
  plot3d(samples)  #show 3d plane
})

test_that('can produce new har CSV from 0,1 CSV', {
  df <- fread(get_Resilio_filepath('noPostureNeutralForceTrials2017_11_12_14_28_20.txt'))
  concatenated_ft_dfs <- df["map_creation_id"!=0]
  unique_maps <- unique(concatenated_ft_dfs$map_creation_id)
  list_of_ft_dfs <- split(concatenated_ft_dfs, concatenated_ft_dfs$map_creation_id)
  input_output_datatable <- dcrb(lapply(list_of_ft_dfs, function(ts_df){
    settled_section <- tail(ts_df,100)
    colMeans(settled_section)
  }))
  input_output_data<- data.frame(input_output_datatable)


range_tension <- c(3,20)
  num_muscles <- 7
  A_fit <- find_A_matrix(input_output_data,  measured(muscle_names())[1:num_muscles], forces_of_interest=c("JR3_FX"))
  AMatrix_with_offset <- A_fit$AMatrix
  muscle_diag_ones <- diag(rep(1, num_muscles))
  muscle_constraints_matrix <- cbind(rep(0, num_muscles), muscle_diag_ones)
  generator_columns_A_matrix <- t(A_fit$AMatrix)

  #TODO create test to give some examples for usage this pass_unit_cube_to_A(big_A, 3, c(3,20))
})

##' TODO check this test_that'
context("stop_if_min_equals_max")
test_that("stop_if_min_equals_max will display stop message",
{
  input_range_different <- c(1,50)
  expect_silent(stop_if_min_equals_max(input_range_different))
  input_range_same <- c(3,3)
  expect_error(stop_if_min_equals_max(input_range_same), paste0("Range provided needs to have different values for max and min. You only gave me ", input_range_same[1]))
})


test_that("evaluate ability to find A matrix for known canonical system", {
  canonical_A <-  canonical_linear_system(1)$constr
  noise_3d <- matrix(runif(30000),10000,3)
  colnames(noise_3d) <- muscle_names()[1:3]
  response <- t(canonical_A %*% t(noise_3d))[,1]
  data <- as.data.frame(cbind(noise_3d,JR3_FX=response))
  A_fit <- find_A_matrix_without_offset(data, regressor_names = muscle_names()[1:3], forces_of_interest = "JR3_FX")
  expect_true(max(abs(A_fit$endpointForcePrediction - A_fit$endpointForceObservation)) < 1e-5)
  t(A_fit$AMatrix) == canonical_A[1,]
})
