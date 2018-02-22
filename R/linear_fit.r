
##' Posture RDS to Static Training Data
##' Compose training data from a posture RDS (a list of ForceTrials)
##' TODO Create Test or Retire Function'
##' @param rds_file_path string. object must be a list of ForceTrials
##' @param last_n_milliseconds number of samples used to generate column means for static representation in training data
##' @return training_data column of means of training data form a list of ForceTrials'
posture_rds_to_static_training_data <- function(rds_file_path, last_n_milliseconds) {
  list_of_forcetrials <- readRDS(rds_file_path)
  training_data <- converged_colmeans(list_of_forcetrials, last_n_milliseconds = 100)
  return(training_data)
}
##' Posture RDS to A matrix information
##' Uses all data to train
##' TODO Create Test or Retire Function'
##' @param rds_file_path string. object must be a list of ForceTrials
##' @param last_n_milliseconds number of samples used to generate column means for static representation in training data
posture_rds_to_A_matrix <- function(rds_file_path, last_n_milliseconds, fraction_training) {
  input_output_data <- posture_rds_to_static_training_data(rds_file_path, last_n_milliseconds)
  train_test <- df_split_into_training_and_testing(input_output_data, fraction_training)
  find_A_matrix(train_test$train)
}

##' df_split_into_training_and_testing
##' TODO Create Test or Retire Function
##' Does pre-shuffle row-wise randomly before splitting
##' @param input_output_data df of row-observations to train on
##' @param fraction_training between 0 and 1, a numeric
##' @return list_of_train_test list of two dataframes
df_split_into_training_and_testing <- function(input_output_data, fraction_training){
  df_shuffled <- shuffle_row_wise(input_output_data)
  n_training <- floor(nrow(df_shuffled)*fraction_training)
  n_test_set <- nrow(df_shuffled) - n_training
  training_set <- head(df_shuffled, n_training)
  test_set <- tail(df_shuffled, n_test_set)
  return(list(train=training_set, test=test_set))
}

##' map_A_to_x
##' Multiply A matrix by vector x (linear system Ax=b)
##' TODO Create test'
##' @param x vector
##' @param A matrix
##' @param b vector
map_A_to_x <- function(x, A) A %*% x

generate_linear_static_model <- function(input_output_data, fraction_training){
  train_test <- df_split_into_training_and_testing(input_output_data, fraction_training = 0.8)
  model_A_matrix <- find_A_matrix(train_test$train)$AMatrix
  test_inputs  <- train_test$test[,do.call('c',lapply(muscle_names(), measured))]
  test_outputs <- train_test$test[,force_column_names]
  row.names(test_outputs) <- NULL
  row_input_list <- df_to_list_of_rows(as.matrix(test_inputs))
  input_row_vecs <- lapply(row_input_list, function(row) t(as.matrix(row)))
  input_col_vecs <- lapply(input_row_vecs,t)
  predicted_forces_list <- lapply(input_col_vecs, map_A_to_x, matrix(model_A_matrix, nrow=ncol(model_A_matrix)))
  predicted_forces_df <- dcrb(lapply(predicted_forces_list,t))
  colnames(predicted_forces_df) <- force_column_names
  residual_df <- predicted_forces_df - test_outputs
  euclidian_errors <- apply(residual_df[,1:3],1,function(row) norm_vec(row))
  return(list(model_A_matrix = model_A_matrix, train = train_test$train, test = train_test$test, euclidian_errors = euclidian_errors))
}


##' L2 Norm of a Vector'
##" https://stackoverflow.com/questions/10933945/how-to-calculate-the-euclidean-norm-of-a-vector-in-r
##' TODO Create test
##' @param x vector of numeric
##' @return magnitude numeric value'
norm_vec <- function(x) sqrt(sum(x ^ 2))

##' Element-wise difference between 2 vectors
##' TODO test
##' @param known vector
##' @param predicted vector'
##' @return known vector minus predicted vector'
vector_difference <- function(known,predicted){
  return(known - predicted)
}
##' Get Norm of the difference between 2 vectors
##' TODO Create test
##' @param known vector
##" @param predicted vector
norm_vector_difference <- function(known,predicted) norm_vec(vector_difference(known,predicted))

##' Posture RDS list to a list of A matrix information objects
##' Uses all data to train
##' TODO Create test
##' @param rds_file_path_list list of strings, each connects to a file, each containing a list of ForceTrials
##' @param last_n_milliseconds number of samples used to generate column means for static representation in training data
##' @importFrom pbapply pblapply
posture_rds_files_to_list_of_A_matrix_fits <- function(list_of_posture_rds_paths,
  last_n_milliseconds) {
  lapply(list_of_posture_rds_paths, posture_rds_to_A_matrix, last_n_milliseconds, 1.0)
}

##' get_adept_coordinates_from_rds
##' Reads adept coordinates and the first force from an RDS posture string path'
##' TODO Create Test'
##' @param rds_posture_string_path full path to an RDS containing a list of ForceTrials
##' @return adept_coordinates tuple numeric vector of adept_x, adept_y
get_adept_coordinates_from_rds <- function(rds_posture_string_path) {
  first_force_from_posture <- readRDS(rds_posture_string_path)[[1]]
  return(adept_coordinates_from_ForceTrial(first_force_from_posture))
}

##' list_of_xy_to_df
##' Cretes a dataframe with column names from 2 strings'
##' TODO Create test'
##' @param list list of 2-element numeric vectors
##' @param xy_colnames list of 2 strings, which will be the column names for the resultant dataframe
##' @return df dataframe'
list_of_xy_to_df <- function(list, xy_colnames) {
  df <- do.call("rbind", list)
  colnames(df) <- xy_colnames
  return(df)
}

##' Compose a Y~X string for use in titles for linear relationships
##' TODO Create test'
##' @param x string, independent variable name
##' @param y string, response variable name
##' @return xy linear relationship string
is_a_function_of <- function(x,y) paste0(y, "~", x)
