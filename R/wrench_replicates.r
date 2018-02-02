##' Histogram per absolute residual '
##' @param residuals per map a dataframe where each column is a vector of residuals from an expected value. colnames are JR3_FX, etc. rows are the replicates.
##' you can put a dataframe where all rows are the same map, to show within-map replicability
##' or you can put a dataframe where all rows are different map residuals, to describe replicability of force in general, across multiple maps.
histogram_per_absolute_residual_from_vector_dimension <- function(residuals_per_map) {

  par(mfcol = c(3, 2))
  biggest_absolute_residual_from_vector_dimension_mean <- max(apply(residuals_per_map,
    2, function(x) {
      max(abs(x))
    }))
  force_max_abs_residual_from_mean <- max(apply(residuals_per_map[, force_names_to_predict[1:3]],
    2, function(x) {
      max(abs(x))
    }))
  moment_max_abs_residual_from_mean <- max(apply(residuals_per_map[, force_names_to_predict[4:6]],
    2, function(x) {
      max(abs(x))
    }))

    browser()
    melt(residuals_per_map)
    ggplot(residuals_per_map[1], aes(JR3_FX)) + geom_histogram(fill="black") + theme_minimal()



  sapply(colnames(residuals_per_map[1:3]), function(force_dimension_of_interest) {
    hist(residuals_per_map[, force_dimension_of_interest], col = "black",
      main = force_dimension_of_interest, xlim = c(-force_max_abs_residual_from_mean,
        force_max_abs_residual_from_mean), xlab = "residual_from_mean",
      ylab = "Count", breaks = 20)
  })
  sapply(colnames(residuals_per_map[4:6]), function(force_dimension_of_interest) {
    hist(residuals_per_map[, force_dimension_of_interest], col = "black",
      main = force_dimension_of_interest, xlim = c(-moment_max_abs_residual_from_mean,
        moment_max_abs_residual_from_mean), xlab = "residual_from_mean",
      ylab = "Count", breaks = 20)
  })
}


##' @param static_response input_output_data with columns like JR3_FX, and reference_M0
print_latex_table_for_replicate_maps <- function(static_response){
input_replicate_maps <- as.data.frame(dcrb(lapply(split(static_response,
  static_response$reference_M0), tail, 1)))[, reference(muscle_names())]
colnames(input_replicate_maps) <- muscle_biological_names()
rownames(input_replicate_maps) <- 0:4
print("Latex table for hand3ultraflex replicate input maps 0-4")
print(xtable(input_replicate_maps))
}
