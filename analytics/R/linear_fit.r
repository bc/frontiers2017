##' Prepend String
##' This is like paste0, but the arguments are reversed. This way you can use it with lapply.
##' @param b string to put in back
##' @param a string to put in front
##' @return a_and_b string concatenated
prepend_string <- function(b, a) {
  paste0(a, b)
}
##' Posture RDS to Static Training Data
##' Compose training data from a posture RDS (a list of ForceTrials)
##' @param rds_file_path string. object must be a list of ForceTrials
##' @param last_n_milliseconds number of samples used to generate column means for static representation in training data
posture_rds_to_static_training_data <- function(rds_file_path, last_n_milliseconds) {
  list_of_forcetrials <- readRDS(rds_file_path)
  training_data <- converged_colmeans(list_of_forcetrials, last_n_milliseconds = 100)
  return(training_data)
}
##' Posture RDS to A matrix information
##' Uses all data to train
##' @param rds_file_path string. object must be a list of ForceTrials
##' @param last_n_milliseconds number of samples used to generate column means for static representation in training data
posture_rds_to_A_matrix <- function(rds_file_path, last_n_milliseconds) {
  training <- posture_rds_to_static_training_data(rds_file_path, last_n_milliseconds)
  find_A_matrix(training)
}

##' Posture RDS list to a list of A matrix information objects
##' Uses all data to train
##' @param rds_file_path_list list of strings, each connects to a file, each containing a list of ForceTrials
##' @param last_n_milliseconds number of samples used to generate column means for static representation in training data
list_of_posture_rds_files_to_list_of_A_matrices <- function(list_of_posture_rds_paths,
  last_n_milliseconds) {
  lapply(list_of_posture_rds_paths, posture_rds_to_A_matrix, last_n_milliseconds)
}

##' get_adept_coordinates_from_rds
##' @param rds_posture_string_path full path to an RDS containing a list of ForceTrials
##' @return adept_coordinates tuple numeric vector of adept_x, adept_y
get_adept_coordinates_from_rds <- function(rds_posture_string_path) {
  first_force_from_posture <- readRDS(rds_posture_string_path)[[1]]
  return(adept_coordinates_from_ForceTrial(first_force_from_posture))
}

##' list_of_xy_to_df
##' @param list list of 2-element numeric vectors
##' @param xy_colnames list of 2 strings, which will be the column names for the resultant dataframe
list_of_xy_to_df <- function(list, xy_colnames) {
  df <- do.call("rbind", list)
  colnames(df) <- xy_colnames
  return(df)
}


##' posture dependency bi plot
##' just makes plot of settling~delta_tension
##' @param posture_dependency data frame with columns: adepx_x, adept_y, vaf
##' @param independent_variable_name string of the independent variable i.e. 'adept_x'
##' @param response_variable_name string of the response variable i.e. 'vafs'
##' @return ggplot object of the resultant plot
##' @export
##' @importFrom WVPlots ScatterHistC
posture_dependency_plot <- function(posture_dependency, independent_variable_name,
  response_variable_name) {
  linear_relationship_title <- is_a_function_of(independent_variable_name, response_variable_name)
  p <- WVPlots::ScatterHist(posture_dependency, independent_variable_name, response_variable_name,
    smoothmethod = "lm", title = linear_relationship_title,
    annot_size = 1)
  return(p)
}
##' Compose a Y~X string for use in titles for linear relationships
##' @param x string, independent variable name
##' @param y string, response variable name
##' @return xy linear relationship string
is_a_function_of <- function(x,y) paste0(y, "~", x)
