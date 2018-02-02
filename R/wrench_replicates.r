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

##' replicates_to_residuals_of_mean_magnitude
##' This function is used to get an idea of how consistent force production is, when you repeatedly use the same muscle activation pattern. i.e. pulling 10N on all muscles, but repeating this 10 times, and looking at the scatter of the output wrench.
##' @param replicate_results a data.table of 5 observations, each of which is a df where columns are JR3's and reference etc, but each row represents an observation. All rows should have the same reference_M0, M1, etc because they are replicates. nrow is the number of replicates
##' @param force_names_of_interest vector of string elements, by default this is just FX fy and fz, for a Newtons euclidian distance.
##' @return norm_vec_residuals_from_mean_magnitude
replicates_to_residuals_of_mean_magnitude <- function(replicate_results, force_names_of_interest = dots_to_underscores(force_column_names)[1:3]) {
  section <- replicate_results[, force_names_of_interest, with = FALSE]
  norm_vec_residuals_from_mean_magnitude <- dcrb(lapply(df_to_list_of_rows(section), function(x) {
    norm_vec(x) - norm_vec(colMeans(section))}))
    colnames(norm_vec_residuals_from_mean_magnitude) <- "residual_from_mean_magnitude"
    return(norm_vec_residuals_from_mean_magnitude)
  }
##' list_of_replicates_to_replicates_to_residuals_of_mean_magnitude'
##' Applies replicates_to_residuals_of_mean_magnitude to list of different inputs. for instance, the first element of the input should be a data.frame of ONLY one type of input value, examiningthe outputs.
##' @param list_of_replicate_results a list of data.tables, each composed of of N observations, where cols are JR3's and reference etc, but each row represents an observation. All rows should have the same reference_M0, M1, etc because they are replicates. nrow is the number of replicates
##' @return list of data.tables, each with the colname ==  "residual_from_mean_magnitude", with N values (N = num replicates). names of elements are numbered 0 to len(input). order is preserved.
list_of_replicates_to_replicates_to_residuals_of_mean_magnitude <- function(list_of_replicate_results){
  norm_vec_residuals_df <- lapply(list_of_replicate_results, replicates_to_residuals_of_mean_magnitude)
  names(norm_vec_residuals_df) <- 0:(length(list_of_replicate_results)-1)
  return(norm_vec_residuals_df)
}

##' @param dynamic_trials list, output from extract_static_and_dynamic_data
##' @param last_n_milliseconds integer, number of ms to sample from end of ForceTrial
##' @return data.frame of the stable data, where each row is a static reponse to one of the replicates.
forcetrials_to_static_response_df_for_replicates <- function(dynamic_trials,last_n_milliseconds){
  tails <- extract_tails_from_trials(response$dynamic_trials_list, last_n_milliseconds)
  stabilized_means_df <- as.data.frame(dcrb(lapply(tails, colMeans)))
  # after index 32, the section of serial pulls begins.
  static_response <- as.data.frame(stabilized_means_df[1:32, ])
  static_response <- data.table(static_response[order(static_response$reference_M0),])
  return(static_response)
}


##' halve_force_dimension_value_df_into_forces_and_torques'
   # + scale_x_continuous(limits=c(-0.6,0.6), breaks=seq(-0.2,0.2, by = 0.01))
  ##' @param force_dimension_value_df dataframe where one colum is force_dimension, and the other column is some value of interest. This will split the df into two, so there's one with ony forces, and another with only torques.
  ##' @return list of two elements, with same structure as input, but split into $forces and $torques in [[1]] and [[2]].
   halve_force_dimension_value_df_into_forces_and_torques <- function(force_dimension_value_df){
     wrench_names <- dots_to_underscores(force_column_names)
     forces <- force_dimension_value_df[force_dimension_value_df$force_dimension %in% wrench_names[1:3],]
     torques <- force_dimension_value_df[force_dimension_value_df$force_dimension %in% wrench_names[4:6],]
     return(list(forces = forces, torques = torques))
   }


   ##' print_how_many_samples_of_each_map_were_collected
   ##' @param residual_sets_dt dataframes where each element has N rows. we are interested in finding what N is for each of the elements in the input.
   print_how_many_samples_of_each_map_were_collected <- function(residual_sets_dt){
     number_of_samples_per_replicate <- as.vector(dcc(lapply(residual_sets_dt, nrow)))
     message("Number of samples per replicate:")
     message(number_of_samples_per_replicate)
   }


   ##' @param melted_df where first col is the JR3 name of dimension, and the next is the value of interest.
   plot_boxplot_faceted_by_JR3 <- function(melted_df){
     p0 <- ggplot(,aes(map, residual_from_mean, fill=map, label = residual_from_mean))
     p0 <- p0 + geom_boxplot()
     p0 <- p0 + facet_wrap(~force_dimension)
     p0 <- p0 + geom_text()
     p0 <- p0 + xlab("MAP")
     p0 <- p0 + ylab("residual value. N if forces, Nm if torques")
     return(p0)
   }
