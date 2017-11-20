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
##' @param tension_range 2 element vector with min and max, that will be the bounds of the u.a.r. sampling
noise_df <- function(muscles_of_interest, number_of_maps_to_generate, tension_range) {
  noise <- uar_matrix(number_of_maps_to_generate, length(muscles_of_interest), min = tension_range[1], max = tension_range[2])
  cbound <- cbind(generate_map_creation_ids(nrow(noise)), as.data.frame(noise))
  colnames(cbound) <- c("map_creation_id", muscles_of_interest)
  return(cbound)
}
