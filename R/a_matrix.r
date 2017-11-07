
##' This function estimates the A matrix from measured tendon forces and output forces. performs 6D linear fit.
##' @param data input output data that matches measured_muscle_col_names and force_column_names.
##' @return fit_object list of AMatrix, endpointForceObservation, endpointForcePrediction, regressor_means, response_means
find_A_matrix <- function(data, muscles_of_interest=muscle_names(), forces_of_interest=force_column_names) {
  # The regressor matrix is concatenation of tendon forces
  time <- data[[1]]
  num_regressor_columns = length(muscles_of_interest) + 1 #inc regressor
  num_response_columns = length(forces_of_interest)
  measured_muscle_col_names <- simplify2array(lapply(muscles_of_interest, measured))
  regressor <- as.matrix(data[measured_muscle_col_names])
  num_observation <- nrow(regressor)
  vector_one <- as.matrix(rep(1,num_observation),num_observation,1)
  colnames(vector_one) <- 'offset'
  regressor <- cbind(vector_one,regressor)
  endpointForceObservation <- data[forces_of_interest]
  AMatrix <- matrix(solve(qr(regressor, LAPACK = TRUE), endpointForceObservation),
  num_regressor_columns, num_response_columns)
  endpointForcePrediction <- data.frame(regressor %*% AMatrix)
  colnames(endpointForcePrediction) <- forces_of_interest
  colnames(AMatrix) <- forces_of_interest
  rownames(AMatrix) <- colnames(regressor)
  regressor_means <- colMeans(regressor)
  response_means <- colMeans(endpointForceObservation)
  fit <- list(AMatrix = AMatrix, endpointForceObservation = endpointForceObservation,
    endpointForcePrediction = endpointForcePrediction, regressor_means = regressor_means,
  response_means = response_means)
  return(fit)
}

##' Split H matrix from the offset.
##' Takes in result from find_A_matrix
##' @param A_matrix matrix with an offset column, and rest are muscle generators
##' @return H_and_offset
split_H_from_offset <- function(A_matrix){
	H <- as.data.frame(t(A_matrix))
	offset <- H[,'offset']
	H$offset <- NULL
	return(list(H=H, offset = offset))
}


##' Matrix multiply input force by A matrix
##' @param A A matrix cols are jr3.fx, etc, rows are offset, measured m0, ...
##' @param x input forces where cols are measured_force & offset set to ones()
predict_output_force <- function(A, x){
	as.matrix(x %*% A)
}
