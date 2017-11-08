set.seed(100)
range_tension <- c(3, 20)
measured_muscle_col_names <- simplify2array(lapply(muscle_names(), measured))

sample_input_output_data <- read_rds_to_package_extdata("training_data.rds")
data <- df_split_into_training_and_testing(sample_input_output_data, fraction_training = 0.8)
training_data <- data$train
test_data <- data$test


test_that("produce A matrices for different numbers of muscles or output force dimensions",
  {
    pdf("../../../output/A_matrix_fit_tests.pdf", width=15, height = 10*7)
    par(mfrow=c(7,2))
    fits <- list(
      find_A_matrix(training_data, regressor_names = measured(muscle_names()[1]), forces_of_interest=force_column_names[1]),
      find_A_matrix(training_data, regressor_names = measured(muscle_names()[1:2]),forces_of_interest=force_column_names[1]),
      find_A_matrix(training_data, regressor_names = measured(muscle_names()[1:3]),forces_of_interest=force_column_names[1]),
      find_A_matrix(training_data, regressor_names = measured(muscle_names()[1:4]),forces_of_interest=force_column_names[1]),
      find_A_matrix(training_data, regressor_names = measured(muscle_names()[1:5]),forces_of_interest=force_column_names[1]),
      find_A_matrix(training_data, regressor_names = measured(muscle_names()[1:6]),forces_of_interest=force_column_names[1]),
      find_A_matrix(training_data, regressor_names = measured(muscle_names()[1:7]),forces_of_interest=force_column_names[1]),
      find_A_matrix(training_data, regressor_names = measured(muscle_names()[1:7]),forces_of_interest=force_column_names[1]),
      find_A_matrix(training_data, regressor_names = measured(muscle_names()[1:7]),forces_of_interest=force_column_names[1:2]),
      find_A_matrix(training_data, regressor_names = measured(muscle_names()[1:7]),forces_of_interest=force_column_names[1:3]),
      find_A_matrix(training_data, regressor_names = measured(muscle_names()[1:7]),forces_of_interest=force_column_names[1:4]),
      find_A_matrix(training_data, regressor_names = measured(muscle_names()[1:7]),forces_of_interest=force_column_names[1:5]),
      find_A_matrix(training_data, regressor_names = measured(muscle_names()[1:7]),forces_of_interest=force_column_names[1:6])
  )
  lapply(fits, fit_summary, cex =0.2)
  dev.off()

A_fit <- find_A_matrix(training_data, muscles_of_interest=muscle_names()[1], forces_of_interest=force_column_names[1:3])
  })

test_that("we can calculate cond of an A matrix", {
	cond_num <- kappa(A_fit$AMatrix, exact = TRUE)
	expect_true(cond_num > 0 )
	expect_is(cond_num, 'numeric')
})

num_observation <- nrow(test_data)
vector_one <- as.matrix(rep(1, num_observation), num_observation, 1)
colnames(vector_one) <- "offset"
test_input <- cbind(vector_one, as.matrix(test_data[measured_muscle_col_names]))
test_predicted_response <- predict_output_force(A_fit$AMatrix, test_input)
test_observed_response <- test_data[force_column_names]

test_that("residuals from fitted data are appropriate for sample linear fit.", {


	res_test <- test_observed_response - test_predicted_response
	euclidian_errors_test <- apply(res_test[, 1:3], 1, function(row) norm_vec(row))
	hist(euclidian_errors_test, xlab='Euclidian error in N across F_xyz',
	 ylab="Number of responses in test set",
	  main=paste("n = ", length(euclidian_errors_test)), col='black', breaks=12)
})

test_that('residuals from test data are appropriate for linear fit', {
	res_test <- test_observed_response - test_predicted_response
	res <- abs(A_fit$endpointForceObservation - A_fit$endpointForcePrediction)
	expect_true(max(colMeans(res)) < 0.4)
	euclidian_errors <- apply(res[, 1:3], 1, function(row) norm_vec(row))
	hist(euclidian_errors, xlab='Euclidian error in N across F_xyz', ylab="Number of responses in training set", main=paste("n = ", length(euclidian_errors)), col='black', breaks=12)
})

H_and_offset <- split_H_from_offset(A_fit$AMatrix)
H <- H_and_offset[[1]]
offset <- H_and_offset[[2]]
H <- t(A_fit$AMatrix)[1:3, ]

num_generators <- ncol(H) - 1  #exclude offset
num_muscles <- 7
muscle_diag_ones <- diag(rep(1, num_muscles))
muscle_constraints_matrix <- cbind(rep(0, num_muscles), muscle_diag_ones)

offset_constraints_matrix <- rbind(c(1, rep(0, num_muscles)), c(-1, rep(0, num_muscles)))

big_A <- rbind(H, -H, offset_constraints_matrix, muscle_constraints_matrix, -muscle_constraints_matrix)
six_dimensional_task <- c(-26.06362, -1.101871, -10.42662, 0, 0, 0)
big_b <- c(six_dimensional_task, -six_dimensional_task, 1, 1, rep(range_tension[2],
  7), rep(range_tension[1], 7))

constr <- list(constr = big_A, dir = rep("<=", nrow(big_A)), rhs = big_b)
constraints_are_feasible(constr)

state <- har.init(constr)
result <- har.run(state, n.samples = 10000)
samples <- result$samples

lowest_l1_cost_soln <- samples[which.min(rowSums(samples)), ]
highest_l1_cost_soln <- samples[which.max(rowSums(samples)), ]


test_predicted_response <- as.matrix(samples %*% A_fit$AMatrix)

boxplot(test_predicted_response, ylab = "Tension N for FX,FY,FZ, Torque Nm for MX,MY,MZ")
test_observed_response <- test_data[force_column_names]
res_test <- test_observed_response - test_predicted_response
summary(res_test)


parcoord(samples)
plot3d(samples)  #show 3d plane
