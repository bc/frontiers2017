context("Linearity functions manipulations")

set.seed(100)
range_tension <- c(3, 20)
sample_input_output_data <- read_rds_to_package_extdata("training_data.rds")
data <- df_split_into_training_and_testing(sample_input_output_data, fraction_training = 0.8)
training_data <- data$train
test_data <- data$test

context('Computing A Matrix Fits')
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
  summary(res_test)


  parcoord(samples)
  plot3d(samples)  #show 3d plane
})
