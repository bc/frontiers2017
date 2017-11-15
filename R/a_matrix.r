
##' This function estimates the A matrix from measured tendon forces and output forces. performs 6D linear fit.
##' The regressor matrix is concatenation of tendon forces
##' TODO Test Directly
##' @param data input output data that matches measured_muscle_col_names and force_column_names.
##' @return fit_object list of AMatrix, endpointForceObservation, endpointForcePrediction, regressor_means, response_means
find_A_matrix <- function(data, regressor_names = simplify2array(lapply(muscle_names(),
  measured)), forces_of_interest = force_column_names) {
  num_regressor_columns = length(regressor_names) + 1  #inc regressor
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

find_A_matrix_without_offset <- function(data, regressor_names = simplify2array(lapply(muscle_names(),
  measured)), forces_of_interest = force_column_names) {
  num_regressor_columns = length(regressor_names)  #inc regressor
  num_response_columns = length(forces_of_interest)
  regressor <- as.matrix(data[regressor_names])
  endpointForceObservation <- data[forces_of_interest]
  AMatrix <- lin_qr_solve(regressor, endpointForceObservation)
  endpointForcePrediction <- predict_against_input_data(regressor, AMatrix)
  fit <- list(AMatrix = AMatrix, endpointForceObservation = endpointForceObservation,
    endpointForcePrediction = endpointForcePrediction)
  return(fit)
}

##' lin_qr_solve
##' TODO Function Title and description
##' @param x matrix of N independent columns (the regressor), with appropriate colnames
##' @param b matrix of M dependent variables (with appropriate colnames)
##' @return A matrix representing linear least squares regression fit for x matrix -> b
lin_qr_solve <- function(x, b) {
  A <- matrix(solve(qr(x, LAPACK = TRUE), b), ncol(x), ncol(b))
  colnames(A) <- colnames(b)
  rownames(A) <- colnames(x)
  return(A)
}


##' add_offset_vector_to_regressor
##' userful becuase it keeps track of the mean generator (bias) that is not accounted for by the muscles.
##' @param regressor matrix with as many rows as observations.
##' @return regressor_with_col regressor wiht an offset column appended
add_offset_vector_to_regressor <- function(regressor) {
  num_observation <- nrow(regressor)
  vector_one <- as.matrix(rep(1, num_observation), num_observation, 1)
  colnames(vector_one) <- "offset"
  regressor_with_col <- cbind(vector_one, regressor)
  return(regressor_with_col)
}

##' lin_qr_solve
##' @param x matrix of N independent columns (the regressor), with appropriate colnames
##' @param A matrix representing linear least squares regression fit for x matrix -> b (with colnames for outputs)
##' @param b matrix of expected output forces of interest. n cols, n force dimensions
predict_against_input_data <- function(x, A) {
  endpointForcePrediction <- data.frame(x %*% A)
  colnames(endpointForcePrediction) <- colnames(A)
  return(endpointForcePrediction)
}

##' Matrix multiply input force by A matrix
##' @param A A matrix cols are jr3.fx, etc, rows are offset, measured m0, ...
##' @param x input forces where cols are measured_force & offset set to ones()
##' @return b_vectors matrix of the predicted output forces. as many columns as there are output force dimensionsss
predict_output_force <- function(A, x) {
  return(as.matrix(x %*% A))
}
##' observed_predicted_segments3d
##' @param A_fit linear A matrix fit element with endpointForceObservation and endpointForcePrediction
observed_predicted_segments3d <- function(A_fit) {
  endpoints <- interleave_two_dataframes_as_segments(cbind(A_fit$endpointForceObservation,
    A_fit$endpointForcePrediction))
  segments3d(endpoints)
}

##' interleave_two_dataframes_as_segments
##' @param df dataframe with 3 columns for xyz
##' @return df_interleaved df with 3 columns, where every other point is the starting endpoint, and the second (rep) are the ending endpoint.
interleave_two_dataframes_as_segments <- function(df) {
  df_interleaved <- data.frame(x = as.vector(t(markers[, c(1, 4)])), y = as.vector(t(markers[,
    c(2, 5)])), z = as.vector(t(markers[, c(3, 6)])))
  return(df_interleaved)
}

##' Fit Summary
##' Performs summary on the fit residuals and the
##' @param A_fit object as returned from find_A_matrix
##' @param ... params passed to subfunctions
fit_summary <- function(A_fit, ...) {
  hist_force_magnitudes(A_fit$endpointForceObservation, "Training data observations")
  hist_force_magnitudes(A_fit$endpointForcePrediction, "Fit predictions on training data")
  magnitude_of_residuals <- magnitudes(abs(A_fit$endpointForceObservation - A_fit$endpointForcePrediction))
  forces_of_interest <- paste(colnames(A_fit$endpointForcePrediction), collapse = ", ")
  regressor_names <- paste(rownames(A_fit$AMatrix), collapse = ", ")
  message("General summary of the observed training data")
  print(summary(A_fit$endpointForceObservation))
  print(column_ranges(A_fit$endpointForceObservation))

  message("Euclidian errors within the training set:")
  hist_euclidian_errors(magnitude_of_residuals, forces_of_interest, regressor_names = regressor_names,
    source_of_vals = "training set")
  print(summary(magnitude_of_residuals))
}

##' hist_euclidian_errors
##' @param forces_of_interest forces used in the fit
##' @param regressor_names regressors used in the fit, i.e. c('measured_M0', 'measured_M1', ...)
##' @param euclidian_errors_vector vector of strictly positive magnitudes, in N.
##' @param source_of_vals string of the source, i.e. 'training set.'
##' @param ... params passed to subfunctions
hist_euclidian_errors <- function(euclidian_errors_vector, forces_of_interest, regressor_names,
  source_of_vals, ...) {
  hist(euclidian_errors_vector, xlab = paste("Euclidian error in N across",
    forces_of_interest), ylab = paste("Number of responses in ", source_of_vals),
    main = paste("n = ", length(euclidian_errors_vector), ", Regressors = ",
      paste0(regressor_names, collapse = ",")), col = "black", breaks = 12,
    ...)
}

##' Table of show min max of each column
##' TODO Test Mayumi
##' @param df data frame with n columns
##' @return range_df data.frame of the min and max of each column
column_ranges <- function(df) {
  range_df <- t(apply(df, 2, range))
  colnames(range_df) <- c("min", "max")
  return(range_df)
}
##' Fit Evaluation
##' @param A_fit object as returned from find_A_matrix
##' @param test_data dataset with the input and output columns matching the regressors and outputs of A_fit
fit_evaluation <- function(A_fit, test_data, ...) {
  par(mfcol = c(3, 2))
  fit_summary(A_fit)
  evaluate_fit_wrt_test_data(A_fit, test_data)
}

fit_evaluation_without_offset <- function(A_fit, test_data, ...) {
  par(mfcol = c(3, 2))
  fit_summary(A_fit)
  evaluate_fit_wrt_test_data_without_offset(A_fit, test_data)
}
##' evaluate_fit_wrt_test_data
##' @param A_fit result from find_A_matrix
##' @param test_data df with same cols as regressors+response variables as in A_fit
evaluate_fit_wrt_test_data <- function(A_fit, test_data) {
  num_observation <- nrow(test_data)
  regressor_names <- rownames(A_fit$AMatrix)
  regressor_names_without_offset <- regressor_names[regressor_names!='offset']
  force_col_names <- colnames(A_fit$AMatrix)
  forces_of_interest <- paste0(force_col_names, collapse=",")
  vector_one <- as.matrix(rep(1, num_observation), num_observation, 1)
  colnames(vector_one) <- "offset"
  test_input <- cbind(vector_one, as.matrix(test_data[regressor_names_without_offset]))
  test_predicted_response <- predict_output_force(A_fit$AMatrix, test_input)
  test_observed_response <- test_data[force_col_names]
  hist_force_magnitudes(test_observed_response, "Force observations from test data ")
  hist_force_magnitudes(test_predicted_response, "Force Predictions from test data")
  res_test <- test_observed_response - test_predicted_response
  hist_euclidian_errors(magnitudes(res_test), forces_of_interest, regressor_names,
    source_of_vals="test data")
  message('Summary of residuals when predicting model against test set')
  print(summary(res_test))
  print(column_ranges(res_test))
  message('summary of euclidian magnitudes of residuals in model vs test set ')
  print(summary(magnitudes(res_test)))
}

evaluate_fit_wrt_test_data_without_offset <- function(A_fit, test_data) {
  num_observation <- nrow(test_data)
  regressor_names <- rownames(A_fit$AMatrix)
  force_col_names <- colnames(A_fit$AMatrix)
  forces_of_interest <- paste0(force_col_names, collapse=",")
  vector_one <- as.matrix(rep(1, num_observation), num_observation, 1)
  test_input <- as.matrix(test_data[regressor_names])
  test_predicted_response <- predict_output_force(A_fit$AMatrix, test_input)
  test_observed_response <- test_data[force_col_names]
  hist_force_magnitudes(test_observed_response, "Force observations from test data ")
  hist_force_magnitudes(test_predicted_response, "Force Predictions from test data")
  res_test <- test_observed_response - test_predicted_response
  hist_euclidian_errors(magnitudes(res_test), forces_of_interest, regressor_names,
    source_of_vals="test data")
  message('Summary of residuals when predicting model against test set')
  print(summary(res_test))
  print(column_ranges(res_test))
  message('summary of euclidian magnitudes of residuals in model vs test set ')
  print(summary(magnitudes(res_test)))
}


##' Compute magnitudes for a DF of forces
##' @param force_df dataframe with N force columns for N dimensions of the same units'
##' @param magnitudes vector of numeric magnitude, same length as nrow(force_df)
magnitudes <- function(force_df) {
  as.vector(apply(force_df, 1, norm_vec))
}

##' Histogram of force magnitudes for XYZ
##' @param force_df dataframe with N force columns for N dimensions of the same units'
hist_force_magnitudes <- function(force_df, condition = "") {
  magnitudes <- magnitudes(force_df)
  force_dimensions_xlab <- paste("Force magnitude in", colnames(force_df), "(N)",collapse=",")
  hist(magnitudes, breaks = 20, col = "black", xlab = force_dimensions_xlab,
    main = paste("n=", length(magnitudes), ",", condition))
}
##' N Binary combinations'
##' https://stackoverflow.com/questions/18705153/generate-list-of-all-possible-combinations-of-elements-of-vector
##' @param n length of the vector
##' @return M matrix where each row is a unique combination of 0 and 1
n_binary_combinations <- function(n) {
  M <- as.matrix(expand.grid(rep(list(0:1), n)))
  dimnames(M) <- NULL
  return(M)
}
##' Custom Binary combinations
##' @param n length of the vector
##' @param tension_range the range that the inputs can be at
##' @return M matrix where each row is a unique combination of tension_range[1] min and tension_range[2] max
custom_binary_combinations <- function(n, tension_range){
  stop_if_min_equals_max(tension_range)
  mat <- n_binary_combinations(n)
  mask_for_ones <- mat == 1
  mask_for_zeros <- mat == 0
  mat[,] <- NA
  mat[mask_for_ones] <- tension_range[1]
  mat[mask_for_zeros] <- tension_range[2]
  dimnames(mat) <- NULL
  return(mat)
}

##' Custom Binary combinations
##' Creates a set of unique identifiers for when the MAP was created.
##' use this to see what time the MAP was created in your time zone. E.g.
##' https://www.wolframalpha.com/input/?i=unixtime+1510379473228.8129883*1e-3+to+PST
##' @param n length of the vector
##' @param tension_range the range that the inputs can be at
##' @return M matrix where each row is a unique combination of tension_range[1] min and tension_range[2] max
compose_binary_combination_df <- function(n,tension_range){
  newton_values <- custom_binary_combinations(n, tension_range)
  id_vec <- format(dcc(lapply(1:nrow(newton_values), function(x) as.numeric(Sys.time())*1000)), digits=16)
  reference_value_colnames <- paste0("M",0:(n-1))
  colnames(newton_values) <- reference_value_colnames
  df <- cbind(map_creation_id = id_vec, newton_values)
  rownames(df) <- c()
  return(df)
}
##' generate_map_creation_ids
##' @param n number of ids to create
##' @return m vector of map_creation_id strings (unixtime in ms with 16 digits)
generate_map_creation_ids <- function(n){
  return(format(dcc(lapply(1:n, function(x) as.numeric(Sys.time())*1000)), digits=16))
}
##' Custom Binary combinations to CSV, ready for input to NI cpu
##' Creates a set of unique identifiers for when the MAP was created.
##' use this to see what time the MAP was created in your time zone. E.g.
##' https://www.wolframalpha.com/input/?i=unixtime+1510379473228.8129883*1e-3+to+PST
##' @param n length of the vector
##' @param tension_range the range that the inputs can be at
##' @param filename desired output directory filepath
write_binary_combination_csv <- function(n, tension_range, filename){
  df <- compose_binary_combination_df(n,tension_range)
  write.csv(df, filename, row.names=FALSE,quote=FALSE)
}


##' stop_if_min_equals_max
##' @param input_range vector of two values indicating c(min,max)
stop_if_min_equals_max <- function(input_range){
  if (input_range[1]==input_range[2]){
    stop(paste("Range provided needs to have different values for max and min. You only gave me ",input_range[1]))
  }
}

##' Pass unit cube to A.
##' useful to see what is achieved when running combinations of minimal and maximal values of all muscles.
##' @param num_output_dimensions an integer, i.e. 3 if you want to get Fx, Fy and Fz
##' @param big_A matrix representing translation of tensions into forces. includes offset vector
pass_unit_cube_to_A <- function(big_A, num_output_dimensions, tension_range){
  output_b_matrix <- big_A %*% t(left_pad_ones(custom_binary_combinations(ncol(big_A)-1,tension_range)))
  forces<- t(output_b_matrix[1:num_output_dimensions,])
  return(forces)
}
