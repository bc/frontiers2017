
##' This function estimates the A matrix from measured tendon forces and output forces. performs 6D linear fit.
##' @param data input output data that matches measured_muscle_col_names and force_column_names.
##' @return fit_object list of AMatrix, endpointForceObservation, endpointForcePrediction, regressor_means, response_means
find_A_matrix <- function(data, regressor_names = simplify2array(lapply(muscle_names(), measured)), forces_of_interest=force_column_names) {
  # The regressor matrix is concatenation of tendon forces
  time <- data[[1]]
  num_regressor_columns = length(regressor_names) + 1 #inc regressor
  num_response_columns = length(forces_of_interest)
  regressor <- as.matrix(data[regressor_names])
  regressor <- add_offset_vector_to_regressor(regressor)
  endpointForceObservation <- data[forces_of_interest]
  AMatrix <- lin_qr_solve(regressor, endpointForceObservation)
  endpointForcePrediction <- predict_against_input_data(regressor, AMatrix)
  fit <- list(AMatrix = AMatrix, endpointForceObservation = endpointForceObservation,
    endpointForcePrediction = endpointForcePrediction)
  return(fit)
}

##' lin_qr_solve
##' @param x matrix of N independent columns (the regressor), with appropriate colnames
##' @param b matrix of M dependent variables (with appropriate colnames)
##' @return A matrix representing linear least squares regression fit for x matrix -> b
lin_qr_solve <- function(x, b){
  A <- matrix(solve(qr(x, LAPACK = TRUE), b),
  ncol(x), ncol(b))
  colnames(A) <- colnames(b)
  rownames(A) <- colnames(x)
  return(A)
}


##' add_offset_vector_to_regressor
##' userful becuase it keeps track of the mean generator (bias) that is not accounted for by the muscles.
##' @param regressor matrix with as many rows as observations.
##' @return regressor_with_col regressor wiht an offset column appended
add_offset_vector_to_regressor <- function(regressor){
  num_observation <- nrow(regressor)
  vector_one <- as.matrix(rep(1,num_observation),num_observation,1)
  colnames(vector_one) <- 'offset'
  regressor_with_col <- cbind(vector_one,regressor)
  return(regressor_with_col)
}

##' lin_qr_solve
##' @param x matrix of N independent columns (the regressor), with appropriate colnames
##' @param A matrix representing linear least squares regression fit for x matrix -> b (with colnames for outputs)
##' @param b matrix of expected output forces of interest. n cols, n force dimensions
predict_against_input_data <- function(x, A){
  endpointForcePrediction <- data.frame(x %*% A)
  colnames(endpointForcePrediction) <- colnames(A)
  return(endpointForcePrediction)
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

fit_summary <- function(A_fit, ...) {
  res <- abs(A_fit$endpointForceObservation - A_fit$endpointForcePrediction)
  euclidian_errors_train <- apply(res, 1, function(row) norm_vec(row))
  forces_of_interest <- paste(colnames(A_fit$endpointForcePrediction), collapse=", ")
  hist(euclidian_errors_train, xlab=paste('Euclidian error in N across F_', forces_of_interest)),
  ylab="Number of responses in training set.",
  main=paste("n = ", length(euclidian_errors_train),", Regressors = ",paste0(rownames(A_fit$AMatrix), collapse=",")), col='black', breaks=12, ...)
  return(summary(euclidian_errors_train))
}
##'' Compute magnitudes for a DF of forces
##' @param force_df dataframe with N force columns for N dimensions of the same units'
##' @param magnitudes vector of numeric magnitude
magnitudes <- function(force_df){
  as.vector(apply(force_df,1, norm_vec))
}

##' Histogram of force magnitudes for XYZ
##' @param force_df dataframe with N force columns for N dimensions of the same units'
hist_force_magnitudes <- function(force_df, condition=""){
  magnitudes <- magnitudes(force_df)
  hist(magnitudes, breaks=20, col='black', xlab="Force magnitude in xyz (N)", main=paste("n=", length(magnitudes), ",", condition))
}
