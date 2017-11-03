load_all()
training_data <- read_rds_to_package_extdata('training_data.rds')
train_test <- df_split_into_training_and_testing(training_data,
												 fraction_training=0.8)

training_data <- train_test[[1]]
test_data <- train_test[[2]]


A_fit <- find_A_matrix(training_data)
kappa(A_fit$AMatrix, exact=TRUE)
measured_muscle_col_names <- simplify2array(lapply(muscle_names(), measured))
num_observation <- nrow(test_data)
vector_one <- as.matrix(rep(1,num_observation),num_observation,1)
colnames(vector_one) <- 'offset'
test_input <- cbind(vector_one, as.matrix(test_data[measured_muscle_col_names]))
test_endpointForcePrediction <- as.matrix(test_input %*% A_fit$AMatrix)
test_endpointForceObservation <- test_data[force_column_names]

#Inspect Residuals
res_test <- test_endpointForceObservation - test_endpointForcePrediction
summary(res_test)
res <- A_fit$endpointForceObservation - A_fit$endpointForcePrediction


test_that("unimplemented",{
	expect_equal(5,5)
	})


## library(devtools)
## load_all()
## run the test  a matrix test commands

# To add a breakpoint, use browser(). put browser() anywehre you want to stop.
# type c to exit the browser
# type n to run the next line
