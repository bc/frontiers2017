##' u.a.r matrix
##' @param rows integer, number of rows (maps)
##' @param cols integer, number of columns (muscles)
##' @return matrix
uar_matrix <- function(rows, cols, min, max) {
  matrix(runif(rows * cols, min = min, max = max), ncol = cols)
}
##' generate Noise Dataframe with map_creation_ids
##' a muscle activation pattern(map) is a N dimensional vector of newtons that we apply to a set of N muscles.
##' @param number_of_maps_to_generate int, how many unique muscle activation patterns should we create?
##' @param range_tension 2 element vector with min and max, that will be the bounds of the u.a.r. sampling
noise_df <- function(muscles_of_interest, number_of_maps_to_generate, range_tension) {
  noise <- uar_matrix(number_of_maps_to_generate, length(muscles_of_interest), min = range_tension[1], max = range_tension[2])
  cbound <- cbind(generate_map_creation_ids(nrow(noise)), as.data.frame(noise))
  colnames(cbound) <- c("map_creation_id", muscles_of_interest)
  return(cbound)
}

noise_df_for_only_muscle_of_interest <- function(muscle_of_interest, n_maps_per_muscle, range_tension=range_tension){
  dense_mat <- noise_df(muscle_names(), n_maps_per_muscle, range_tension=range_tension)
  all_except_muscle_of_interest <- muscle_names()[muscle_names()!=muscle_of_interest]
  dense_mat[,all_except_muscle_of_interest] <- rep(0, nrow(dense_mat))
  return(dense_mat)
}

##' noise_df_for_only_muscle_of_interest'
##' to check random forces for one muscle at a time. This is a good way to understand how muscles work when they are completely independent.
##' df result ordered from first muscle to last muscle.
##' TODO remove hard coding of the muscle_names(), and allow N muscles to be used.
##' @param n_maps_per_muscle number of muscle activation patterns to create, for each muscle.
##' @inheritParams noise_df
##' @return noise_df dataframe where each col is a muscle and each row is a map. output length == n_maps_per_muscle*num_muscles
noise_df_one_muscle_at_a_time <- function(n_maps_per_muscle, range_tension){
  dcrb(lapply(muscle_names(), noise_df_for_only_muscle_of_interest, n_maps_per_muscle=100, range_tension = range_tension))
}
