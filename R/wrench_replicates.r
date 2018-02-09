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
  ggplot(residuals_per_map[1], aes(JR3_FX)) + geom_histogram(fill = "black") +
    theme_minimal()



  sapply(colnames(residuals_per_map[1:3]), function(force_dimension_of_interest) {
    hist(residuals_per_map[, force_dimension_of_interest], col = "black", main = force_dimension_of_interest,
      xlim = c(-force_max_abs_residual_from_mean, force_max_abs_residual_from_mean),
      xlab = "residual_from_mean", ylab = "Count", breaks = 20)
  })
  sapply(colnames(residuals_per_map[4:6]), function(force_dimension_of_interest) {
    hist(residuals_per_map[, force_dimension_of_interest], col = "black", main = force_dimension_of_interest,
      xlim = c(-moment_max_abs_residual_from_mean, moment_max_abs_residual_from_mean),
      xlab = "residual_from_mean", ylab = "Count", breaks = 20)
  })
}


##' @param static_response input_output_data with columns like JR3_FX, and reference_M0
print_latex_table_for_replicate_maps <- function(static_response) {
  responses_split_by_M0 <- split(static_response, static_response$reference_M0)
  input_replicate_maps <- as.data.frame(dcrb(lapply(responses_split_by_M0, tail,
    1)))[, reference(muscle_names())]
  table_of_tensions_per_map <- cbind(map = 0:4, input_replicate_maps)
  colnames(table_of_tensions_per_map) <- c("map", muscle_biological_names())
  message("% Latex table for hand3ultraflex replicate input maps 0-4")
  print(xtable(table_of_tensions_per_map), include.rownames = FALSE)
}

##' replicates_to_residuals_of_mean_magnitude
##' This function is used to get an idea of how consistent force production is, when you repeatedly use the same muscle activation pattern. i.e. pulling 10N on all muscles, but repeating this 10 times, and looking at the scatter of the output wrench.
##' @param replicate_results a data.table of 5 observations, each of which is a df where columns are JR3's and reference etc, but each row represents an observation. All rows should have the same reference_M0, M1, etc because they are replicates. nrow is the number of replicates
##' @param force_names_of_interest vector of string elements, by default this is just FX fy and fz, for a Newtons euclidian distance.
##' @return norm_vec_residuals_from_mean_magnitude
replicates_to_residuals_of_mean_magnitude <- function(replicate_results, force_names_of_interest = dots_to_underscores(force_column_names)[1:3]) {
  section <- replicate_results[, force_names_of_interest, with = FALSE]
  norm_vec_residuals_from_mean_magnitude <- dcrb(lapply(df_to_list_of_rows(section),
    function(x) {
      norm_vec(x) - norm_vec(colMeans(section))
    }))
  colnames(norm_vec_residuals_from_mean_magnitude) <- "residual_from_mean_magnitude"
  return(norm_vec_residuals_from_mean_magnitude)
}
##' list_of_replicates_to_replicates_to_residuals_of_mean_magnitude'
##' Applies replicates_to_residuals_of_mean_magnitude to list of different inputs. for instance, the first element of the input should be a data.frame of ONLY one type of input value, examiningthe outputs.
##' @param list_of_replicate_results a list of data.tables, each composed of of N observations, where cols are JR3's and reference etc, but each row represents an observation. All rows should have the same reference_M0, M1, etc because they are replicates. nrow is the number of replicates
##' @return list of data.tables, each with the colname ==  'residual_from_mean_magnitude', with N values (N = num replicates). names of elements are numbered 0 to len(input). order is preserved.
list_of_replicates_to_replicates_to_residuals_of_mean_magnitude <- function(list_of_replicate_results) {
  norm_vec_residuals_df <- lapply(list_of_replicate_results, replicates_to_residuals_of_mean_magnitude)
  names(norm_vec_residuals_df) <- 0:(length(list_of_replicate_results) - 1)
  return(norm_vec_residuals_df)
}

##' @param dynamic_trials_list list, output from extract_static_and_dynamic_data
##' @param last_n_milliseconds integer, number of ms to sample from end of ForceTrial
##' @return data.frame of the stable data, where each row is a static reponse to one of the replicates.
forcetrials_to_static_response_df_for_replicates <- function(dynamic_trials_list,
  last_n_milliseconds) {
  tails <- extract_tails_from_trials(dynamic_trials_list, last_n_milliseconds)
  stabilized_means_df <- as.data.frame(dcrb(lapply(tails, colMeans)))
  # after index 32, the section of serial pulls begins.
  static_response <- as.data.frame(stabilized_means_df[1:32, ])
  static_response <- data.table(static_response[order(static_response$reference_M0),
    ])
  return(static_response)
}


##' halve_force_dimension_value_df_into_forces_and_torques'
# + scale_x_continuous(limits=c(-0.6,0.6), breaks=seq(-0.2,0.2, by = 0.01))
##' @param force_dimension_value_df dataframe where one colum is force_dimension, and the other column is some value of interest. This will split the df into two, so there's one with ony forces, and another with only torques.
##' @return list of two elements, with same structure as input, but split into $forces and $torques in [[1]] and [[2]].
halve_force_dimension_value_df_into_forces_and_torques <- function(force_dimension_value_df) {
  wrench_names <- dots_to_underscores(force_column_names)
  forces <- force_dimension_value_df[force_dimension_value_df$force_dimension %in%
    wrench_names[1:3], ]
  torques <- force_dimension_value_df[force_dimension_value_df$force_dimension %in%
    wrench_names[4:6], ]
  return(list(forces = forces, torques = torques))
}


##' print_how_many_samples_of_each_map_were_collected
##' @param residual_sets_dt dataframes where each element has N rows. we are interested in finding what N is for each of the elements in the input.
print_how_many_samples_of_each_map_were_collected <- function(residual_sets_dt) {
  number_of_samples_per_replicate <- as.vector(dcc(lapply(residual_sets_dt, nrow)))
  message("Number of samples per replicate:")
  vec_len <- length(residual_sets_dt) - 1
  df <- cbind(0:vec_len, number_of_samples_per_replicate)
  colnames(df) <- c("map", "num_replicate_samples_per_map")
  print(df)
}


##' @param melted_df where first col is the JR3 name of dimension, and the next is the value of interest.
plot_boxplot_faceted_by_JR3 <- function(melted_df) {
  p0 <- ggplot(melted_df, aes(map, residual_from_mean))
  p0 <- p0 + geom_boxplot()
  p0 <- p0 + facet_wrap(~force_dimension)
  p0 <- p0 + xlab("MAP")
  p0 <- p0 + ylab("residual value. N if forces, Nm if torques")
  return(p0)
}


##' Plot a histogram of the magnitude residuals
##' Give a sense of how far off a muscle activation pattern's magnitude will be. Useful for energy calculation.
##' Only takes the first 3 force dimensions in N to create the result.
##' @param list_of_replicate_results a list of N data.tables, each with 5 rows. each row is a map trial, each element of the list is a set of map trials that have been grouped by the same muscle activation pattern.
##' @return histogram plot of the spread of residual from the mean XYZ force magnitude.
plot_histogram_of_magnitude_residuals <- function(list_of_replicate_results) {
  list_of_magnitude_residual_dfs <- list_of_replicates_to_replicates_to_residuals_of_mean_magnitude(list_of_replicate_results)
  norm_vec_residuals_df_melt <- melt(list_of_magnitude_residual_dfs)
  colnames(norm_vec_residuals_df_melt) <- c("index_within_replicates", "residual_from_mean_magnitude",
    "map")
  norm_vec_residuals_df_melt$index_within_replicates <- NULL
  norm_vec_residuals_df_melt$residual_from_mean_magnitude <- NULL
  colnames(norm_vec_residuals_df_melt) <- c("residual_from_mean_magnitude", "map")
  p <- ggplot(norm_vec_residuals_df_melt) + geom_histogram(aes(residual_from_mean_magnitude),
    bins = 20)
  p <- p + xlab("Mean force differential from average force for map")
  p <- p + ylab("Number of replicates")
  p <- p + theme_minimal()
  return(p)
}

replicate_results_df_to_mean_6dim_magnitude_scatter <- function(replicate_results_df,
  force_names_to_predict) {
  section <- replicate_results_df[, force_names_to_predict, with = FALSE]
  residuals_from_mean <- dcrb(lapply(df_to_list_of_rows(section), function(x) {
    as.vector(x) - as.vector(colMeans(section))
  }))
  return(residuals_from_mean)
}
##' residual_from_mean_force_dimension_6dim
##' useful for identifying, for a given force dimension, what the scatter
##' in the output force is, even when you use the same muscle activation pattern.
##' @param residual_sets_dt a datatable
##' @return p1 ggplot2 object that facets by forces/torques, showing by-map wrench residual scatter.
residual_from_mean_force_dimension_6dim <- function(residual_sets_dt) {
  names(residual_sets_dt) <- 0:4
  print_how_many_samples_of_each_map_were_collected(residual_sets_dt)
  residual_melt <- melt(residual_sets_dt)
  colnames(residual_melt) <- c("force_dimension", "residual_from_mean", "map")
  split_wrench <- halve_force_dimension_value_df_into_forces_and_torques(residual_melt)
  p1 <- plot_boxplot_faceted_by_JR3(split_wrench$forces)
  p2 <- plot_boxplot_faceted_by_JR3(split_wrench$torques)
  p1 <- arrangeGrob(p1, p2, nrow = 2)
  return(p1)
}

##' Get a df of the static responses to replicate MAP pulls
##' Useful for understanding how the forces are created when you pull on the tendons the same way, multiple times.
##' @param noise_response_wo_null time series df with muscle and force columns at 1kHz.
##' @param last_n_milliseconds integer, last n milliseconds window that defines the range over which to compute the mean value for each column.
##' @return list_of_replicate_results list of dt's each with nrow= number of replicates. length of list is the number of muscle activation patterns that were replicated.
replicate_df_list_from_noise_response <- function(noise_response_wo_null, last_n_milliseconds) {
  replicate_response <- extract_static_and_dynamic_data(noise_response_wo_null,
    group_indices = list(lower = 302, upper = 352), last_n_milliseconds = last_n_milliseconds)
  replicate_stable_df <- forcetrials_to_static_response_df_for_replicates(replicate_response$dynamic_trials_list,
    last_n_milliseconds)
  print_latex_table_for_replicate_maps(replicate_stable_df)
  list_of_replicate_results <- split(replicate_stable_df, replicate_stable_df$reference_M0)
  return(list_of_replicate_results)
}

##' Save the results of replicate MAPs to an RDS file on disk.
##' Saves to the output folder
##' @param list_of_replicate_results list of dt's each with nrow= number of replicates. length of list is the number of muscle activation patterns that were replicated.
write_replicate_results_to_rds <- function(list_of_replicate_results, data_filename,
  last_n_milliseconds) {
  replicate_response_path <- to_output_folder(paste0(data_filename, "_five_map_replicates_clean_static_response_from_tail_",
    last_n_milliseconds, "ms_mean.rds"))
  saveRDS(list_of_replicate_results, replicate_response_path)
}


##' mean square error
##' https://en.wikipedia.org/wiki/Mean_squared_error
##' @param a,b vector of n numbers'
##' @return mse numeric value
mse <- function(a, b) mean((a - b)^2)

##' root mean square error
##' https://en.wikipedia.org/wiki/Root-mean-square_deviation
##' square root of mse result
##' @param a,b vector of n numbers'
##' @return rmse numeric value in same units as original
rmse <- function(a, b) sqrt(mse(a, b))

#3' euclidian_residuals_for_each_map
##' @param list_of_replicate_results list of input_output_data data tables, each with the JR3 columns.
##' @return euclidian_residual_from_mean_per_map list of numeric vectors, each the set of distances from the mean wrench (where only Fxyz are incorporated, moments are ignored.)
euclidian_residuals_for_each_map <- function(list_of_replicate_results) {
  only_fxyz_names <- dots_to_underscores(force_column_names[1:3])
    wrench_mean_per_map <- lapply(list_of_replicate_results, function(x){
      y <- colMeans(as.data.frame(x))[only_fxyz_names]
    })
  euclidian_residual_from_mean_per_map <- lapply(1:5, function(i){
    desired_wrench <- as.numeric(wrench_mean_per_map[[i]])
    as.numeric(dcc(lapply(df_to_list_of_rows(list_of_replicate_results[[i]]), function(e){
      actual_wrench <- as.numeric(as.data.frame(e)[only_fxyz_names])
      euclidian_residual_from_mean <- norm_vec(actual_wrench - desired_wrench)
    })))
  })
  return(euclidian_residual_from_mean_per_map)
}

##' get_mean_euclidian_residual_per_map
##' @param list_of_euclidian_residual_sets_per_map 5 elements, each element is a list of N euclidian distances (numeric values)
##' @return m lapply the means
get_mean_euclidian_residual_per_map <- function(list_of_euclidian_residual_sets_per_map){
  dcc(lapply(list_of_euclidian_residual_sets_per_map, mean))
}

euclidian_residuals_for_replicates_from_noise_response <- function(noise_response_wo_null, last_n_milliseconds, num_replicates_to_use){
  list_of_replicate_results <- lapply(replicate_df_list_from_noise_response(noise_response_wo_null, last_n_milliseconds), tail,5)
  mean_euclidian_residuals_for_replicates <- get_mean_euclidian_residual_per_map(euclidian_residuals_for_each_map(list_of_replicate_results))
  return(mean_euclidian_residuals_for_replicates)
}
