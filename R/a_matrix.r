
# This function estimates the A matrix from the measured tendon force and
# endpoint forces and torques
find_A_matrix <- function(data) {
  # The regressor matrix is concatenation of tendon forces
  time <- data[[1]]
  measured_muscle_col_names <- simplify2array(lapply(muscle_names(), measured))
  raw_regressor <- as.matrix(data[measured_muscle_col_names])
  regressor <- rm_mean_for_multiple_columns(raw_regressor)
  regressor_means <- colMeans(raw_regressor)
  raw_endpointForceObservation <- data[force_column_names]
  endpointForceObservation <- rm_mean_for_multiple_columns(raw_endpointForceObservation)
  response_means <- colMeans(raw_endpointForceObservation)
  AMatrix <- matrix(solve(qr(regressor, LAPACK = TRUE), endpointForceObservation),
  7, 6)
  browser()
  endpointForcePrediction <- data.frame(regressor %*% AMatrix)
  colnames(endpointForcePrediction) <- force_column_names
  colnames(AMatrix) <- force_column_names
  rownames(AMatrix) <- measured_muscle_col_names
  fit <- list(AMatrix = AMatrix, endpointForceObservation = endpointForceObservation,
    endpointForcePrediction = endpointForcePrediction, regressor_means = regressor_means,
  response_means = response_means)
  return(fit)
}