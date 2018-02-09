context('posturedependence_nearest_neighbor')
set.seed(4)

#parameters
muscles_of_interest <- muscle_names()
num_muscles <- length(muscles_of_interest)
force_names_to_predict <- c("JR3_FX","JR3_FY","JR3_FZ","JR3_MX","JR3_MY","JR3_MZ")
range_tension <-  c(0,10)
samples <- hand3_hand4_clean_static_samples()

par(mfcol=c(2,8))
hand3_ultraflex_xy <-nearest_neighbor_fit_eval(samples$hand3_ultraflex, muscles_of_interest, force_names_to_predict)
nearest_neighbor_fit_eval(samples$hand3_flex, muscles_of_interest, force_names_to_predict)
nearest_neighbor_fit_eval(samples$hand3_extend, muscles_of_interest, force_names_to_predict)
nearest_neighbor_fit_eval(samples$hand3_ultraextend, muscles_of_interest, force_names_to_predict)
nearest_neighbor_fit_eval(samples$hand4_ultraflex, muscles_of_interest, force_names_to_predict)
nearest_neighbor_fit_eval(samples$hand4_flex, muscles_of_interest, force_names_to_predict)
nearest_neighbor_fit_eval(samples$hand4_extend, muscles_of_interest, force_names_to_predict)
nearest_neighbor_fit_eval(samples$hand4_ultraextend, muscles_of_interest, force_names_to_predict)

 p1 <- ggplot(as.data.frame(hand3_ultraflex_xy), aes(manhattan_distance_from_neighbors,euclidian_wrench_errors )) + geom_point()
