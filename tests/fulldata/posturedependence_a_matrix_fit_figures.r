context('posture dependency figures for a matrix fits'
)
set.seed(4)
#parameters
muscles_of_interest <- muscle_names()
num_muscles <- length(muscles_of_interest)
force_names_to_predict <- c("JR3_FX","JR3_FY","JR3_FZ","JR3_MX","JR3_MY","JR3_MZ")
range_tension <-  c(0,10)
samples <- hand3_hand4_clean_samples()
A_fit_list <- calculate_and_display_A_fit_per_sample(samples, muscles_of_interest, force_names_to_predict, range_tension)

  ##' get the distance between a map_input of interest and all maps within the training set.
  map_neighbor_distances <- function(map_training_data, map_input){
    as.numeric(apply(map_training_data,1, function(realdata){
      norm_vector_difference(realdata, map_input)
    }))
  }
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
par(mfcol=c(2,8))
hand3_ultraflex_xy<-nearest_neighbor_fit_eval(samples$hand3_ultraflex, muscles_of_interest, force_names_to_predict)
nearest_neighbor_fit_eval(samples$hand3_flex, muscles_of_interest, force_names_to_predict)
nearest_neighbor_fit_eval(samples$hand3_extend, muscles_of_interest, force_names_to_predict)
nearest_neighbor_fit_eval(samples$hand3_ultraextend, muscles_of_interest, force_names_to_predict)
nearest_neighbor_fit_eval(samples$hand4_ultraflex, muscles_of_interest, force_names_to_predict)
nearest_neighbor_fit_eval(samples$hand4_flex, muscles_of_interest, force_names_to_predict)
nearest_neighbor_fit_eval(samples$hand4_extend, muscles_of_interest, force_names_to_predict)
nearest_neighbor_fit_eval(samples$hand4_ultraextend, muscles_of_interest, force_names_to_predict)

ggplot(as.data.frame(hand3_ultraflex_xy), aes(manhattan_distance_from_neighbors,euclidian_wrench_errors )) + geom_point()
