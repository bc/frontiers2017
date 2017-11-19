context("test_force_vector_replicability.r")

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
##' @param muscle_range 2 element vector with min and max, that will be the bounds of the u.a.r. sampling
noise_df <- function(muscles_of_interest, number_of_maps_to_generate, muscle_range) {
  noise <- uar_matrix(number_of_maps_to_generate, length(muscle_of_interest), min = muscle_range[1], max = muscle_range[2])
  cbound <- cbind(generate_map_creation_ids(nrow(noise)), as.data.frame(noise))
  colnames(cbound) <- c("map_creation_id", muscle_of_interest)
  return(cbound)
}
only_show_5_digits <- function(unique_maps, muscles_of_interest){
  muscle_cols <- unique_maps[,muscles_of_interest]
  formatted_muscle_cols <- apply(muscle_cols, 2, format, digits=3)
  browser()
  cbind(unique_maps['map_creation_id'], formatted_muscle_cols)
}

muscle_of_interest <- muscle_names()
tension_range <- c(0,20)
num_replicates <- 30
num_maps <- 100
unique_maps <- noise_df(muscles_of_interest, num_maps, tension_range)
only_show_5_digits(unique_maps)
replicates <- dcrb(lapply(1:num_replicates, function(x) unique_maps))
rep_sh <- shuffle_row_wise(replicates)
write.csv(rep_sh, "uar_noise_nov16.csv",
  row.names = FALSE, quote = FALSE)
