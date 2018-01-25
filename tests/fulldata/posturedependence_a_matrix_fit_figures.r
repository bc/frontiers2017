context('posture dependency figures for a matrix fits'
)
set.seed(4)
#parameters
muscles_of_interest <- muscle_names()
num_muscles <- length(muscles_of_interest)
force_names_to_predict <- c("JR3_FX","JR3_FY","JR3_FZ","JR3_MX","JR3_MY","JR3_MZ")
range_tension <-  c(0,10)
#load and annotate data by hand # and posture
hand4_ultraextend <- read.csv("output/hand4_ultraextend_clean_static_response_from_tail_100ms_mean.csv")
hand4_extend <- read.csv("output/hand4_extend_clean_static_response_from_tail_100ms_mean.csv")
hand4_flex <- read.csv("output/hand4_flex_clean_static_response_from_tail_100ms_mean.csv")
hand4_ultraflex <- read.csv("output/hand4_ultraflex_clean_static_response_from_tail_100ms_mean.csv")
hand3_ultraextend <- read.csv("output/hand3_ultraextend_clean_static_response_from_tail_100ms_mean.csv")
hand3_extend <- read.csv("output/hand3_extend_clean_static_response_from_tail_100ms_mean.csv")
hand3_flex <- read.csv("output/hand3_flex_clean_static_response_from_tail_100ms_mean.csv")
hand3_ultraflex <- read.csv("output/hand3_ultraflex_clean_static_response_from_tail_100ms_mean.csv")

attr(hand4_ultraextend, "hand_number") <- 4
attr(hand4_extend, "hand_number") <- 4
attr(hand4_flex, "hand_number") <- 4
attr(hand4_ultraflex, "hand_number") <- 4
attr(hand3_ultraextend, "hand_number") <- 3
attr(hand3_flex, "hand_number") <- 3
attr(hand3_extend, "hand_number") <- 3
attr(hand3_ultraextend, "hand_number") <- 3

attr(hand4_ultraextend, "posture") <- "ultraextend"
attr(hand4_extend, "posture") <- "extend"
attr(hand4_flex, "posture") <- "flex"
attr(hand4_ultraflex, "posture") <- "ultraflex"
attr(hand3_ultraextend, "posture") <- "ultraextend"
attr(hand3_flex, "posture") <- "flex"
attr(hand3_extend, "posture") <- "extend"
attr(hand3_ultraextend, "posture") <- "ultraextend"

samples <- list(hand4_ultraextend,
hand4_extend,
hand4_flex,
hand4_ultraflex,
hand3_ultraextend,
hand3_flex,
hand3_extend,
hand3_ultraextend)


show_3d_plot_and_save_fit <- function(input_output_data, dataset_name, muscles_of_interest, force_names_to_predict, range_tension) {
  A_fit <- A_fit_from_80_20_split(input_output_data, muscles_of_interest, force_names_to_predict)
  num_muscles <- length(muscles_of_interest)
  generator_columns_A_matrix <- t(t(A_fit$AMatrix) %*% diag(num_muscles))
  # Here, identify a force vector of interest and apply it to the generated A
  binary_combinations <- custom_binary_combinations(num_muscles,range_tension)
  binary_combination_ffs_points <- binary_combinations %*% generator_columns_A_matrix
  um <- read_rds_from_package_extdata('um.rds')
  aspect3d(1/5,1/5,1/5); par3d(windowRect=c(0,0,20000,20000))
  plot_ffs_with_vertices(binary_combination_ffs_points[,1:3], generator_columns_A_matrix[,1:3], alpha_transparency=0.25, range_tension=range_tension)
  points3d(input_output_data[,force_names_to_predict][,1:3], size=1, col="black", alpha=1)
  title3d(main="FFS", xlab="Fx", ylab="Fy", zlab="Fz", col="black")
  view3d(userMatrix = um, zoom=0.75)
}
show_3d_plot_and_save_fit(hand3_ultraflex,dataset_name="hand3_ultraflex",  muscles_of_interest, force_names_to_predict, range_tension)
show_3d_plot_and_save_fit(hand3_flex,dataset_name="hand3_flex",  muscles_of_interest, force_names_to_predict, range_tension)
show_3d_plot_and_save_fit(hand3_extend,dataset_name="hand3_extend",  muscles_of_interest, force_names_to_predict, range_tension)
show_3d_plot_and_save_fit(hand3_ultraextend,dataset_name="hand3_ultraextend",  muscles_of_interest, force_names_to_predict, range_tension)
rgl.open()
show_3d_plot_and_save_fit(hand4_ultraflex,dataset_name="hand4_ultraflex",  muscles_of_interest, force_names_to_predict, range_tension)
show_3d_plot_and_save_fit(hand4_flex,dataset_name="hand4_flex",  muscles_of_interest, force_names_to_predict, range_tension)
show_3d_plot_and_save_fit(hand4_extend,dataset_name="hand4_extend",  muscles_of_interest, force_names_to_predict, range_tension)
show_3d_plot_and_save_fit(hand4_ultraextend,dataset_name="hand4_ultraextend",  muscles_of_interest, force_names_to_predict, range_tension)


  ##' get the distance between a map_input of interest and all maps within the training set.
  map_neighbor_distances <- function(map_training_data, map_input){
    as.numeric(apply(map_training_data,1, function(realdata){
      norm_vector_difference(realdata, map_input)
    }))
  }

  ##' @return map_row integer map row of the training set muscle activation that was closest to the map of interest
  get_map_row_of_training_with_smallest_distance_to_vec <- function(training_data, vec){
    which.min(nearest_map_neighbor(training_data, vec))
  }


nearest_neighbor_fit_eval <- function(input_output_data, muscles_of_interest, force_names_to_predict){
  data <- df_split_into_training_and_testing(input_output_data, fraction_training = 0.80)
  training_data <- data$train
  test_data <- data$test
  nearest_neighbor_indices <- dcc(pblapply(df_to_list_of_rows(test_data[,reference(muscles_of_interest)]), function(test_input){
    get_map_row_of_training_with_smallest_distance_to_vec(training_data[,reference(muscles_of_interest)], test_input)
  }))
 manhattan_distance_from_neighbors <- as.numeric(rowSums(abs(training_data[nearest_neighbor_indices,reference(muscles_of_interest)] - test_data[,reference(muscles_of_interest)])))
 par(mfrow=c(2,1))
 hist(manhattan_distance_from_neighbors, breaks=15, col='black', main="for test input, how far is it from nearest neighbor training input")
 euclidian_wrench_errors <- as.numeric(apply(training_data[nearest_neighbor_indices,force_names_to_predict] - test_data[,force_names_to_predict],1,norm_vec))
 hist(euclidian_wrench_errors, main="Euclidian error [N] across 6D wrench for nearest neighbor", breaks=15, col='black')
}

nearest_neighbor_fit_eval(hand4_ultraextend, muscles_of_interest, force_names_to_predict)
