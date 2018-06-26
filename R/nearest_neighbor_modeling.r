##' get the distance between a map_input of interest and all maps within the training set.
map_neighbor_distances <- function(map_training_data, map_input){
  as.numeric(apply(map_training_data,1, function(realdata){
    norm_vector_difference(realdata, map_input)
  }))
}
##' K-nearest neighbor interpolation where K=1
##' Get the closest input neighbor via euclidian neighbor distances. euclidian is implemented with norm_vector_difference.
##' @param training_data only the training data reference values
##' @param vec the test_input reference values
##' @return map_row integer map row of the training set muscle activation that was closest to the map of interest
get_map_row_of_training_with_smallest_distance_to_vec <- function(training_data, vec){
  which.min(map_neighbor_distances(training_data, vec))
}


nearest_neighbor_fit_eval <- function(input_output_data, muscles_of_interest, force_names_to_predict){
  data <- df_split_into_training_and_testing(input_output_data, fraction_training = 0.80)
  training_data <- data$train
  test_data <- data$test
  nearest_neighbor_indices <- dcc(pbmclapply(df_to_list_of_rows(test_data[,reference(muscles_of_interest)]), function(test_input){
    get_map_row_of_training_with_smallest_distance_to_vec(training_data[,reference(muscles_of_interest)], test_input)
  }))
 manhattan_distance_from_neighbors <- as.numeric(rowSums(abs(training_data[nearest_neighbor_indices,reference(muscles_of_interest)] - test_data[,reference(muscles_of_interest)])))
 hist(manhattan_distance_from_neighbors, breaks=15, col='black', main="for test input, how far is it from nearest neighbor training input")
 euclidian_wrench_errors <- as.numeric(apply(training_data[nearest_neighbor_indices,force_names_to_predict] - test_data[,force_names_to_predict],1,norm_vec))
 hist(euclidian_wrench_errors, main="Euclidian error [N] across 6D wrench for nearest neighbor", breaks=15, col='black')
 return(list(manhattan_distance_from_neighbors=manhattan_distance_from_neighbors, euclidian_wrench_errors=euclidian_wrench_errors))
}
