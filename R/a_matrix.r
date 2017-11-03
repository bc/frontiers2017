
# This function estimates the A matrix from the measured tendon force and
# endpoint forces and torques
find_A_matrix <- function(data) {
  # The regressor matrix is concatenation of tendon forces
  time <- data[[1]]
  measured_muscle_col_names <- simplify2array(lapply(muscle_names(), measured))
  regressor <- as.matrix(data[measured_muscle_col_names])
  num_observation <- nrow(regressor)
  vector_one <- as.matrix(rep(1,num_observation),num_observation,1)
  colnames(vector_one) <- 'offset'
  regressor <- cbind(vector_one,regressor)
  endpointForceObservation <- data[force_column_names]
  AMatrix <- matrix(solve(qr(regressor, LAPACK = TRUE), endpointForceObservation),
  8, 6)
  endpointForcePrediction <- data.frame(regressor %*% AMatrix)
  colnames(endpointForcePrediction) <- force_column_names
  colnames(AMatrix) <- force_column_names
  rownames(AMatrix) <- colnames(regressor)
  regressor_means <- colMeans(regressor)
  response_means <- colMeans(endpointForceObservation)
  fit <- list(AMatrix = AMatrix, endpointForceObservation = endpointForceObservation,
    endpointForcePrediction = endpointForcePrediction, regressor_means = regressor_means,
  response_means = response_means)
  return(fit)
}
