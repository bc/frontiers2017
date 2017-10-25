##' get_reference_to_variance_relationship_df
##' @param force_list list of force time trials
##' @param last_n_milliseconds the number of tail milliseconds from which we should calculate the settled standard deviation.
##' @param muscle the muscle of interest in a string e.g. 'M0'
##' @return reference_standard_deviation df with reference (desired) muscle tension, variance, maximum residual
get_reference_to_variance_relationship_df <- function(force_list, last_n_milliseconds,
  muscle) {
  ref_sd_df <- do.call("rbind", lapply(forces, function(x) {
    ref <- tail(x, 1)[, reference(muscle)]
    measured_tail <- tail(x, last_n_milliseconds)[, measured(muscle)]
    c(ref, sd(measured_tail), max(abs(range(measured_tail) - ref)))
  }))
  colnames(ref_sd_df) <- c("reference", "sd_of_tail", "max_residual")
  rownames(ref_sd_df) <- NULL
  return(ref_sd_df)
}
##' force_trial_to_stable_metrics
##' @param force_trial force trial time series
##' @param last_n_milliseconds the number of tail milliseconds from which we should calculate the settled standard deviation.
##' @param muscle the muscle of interest in a string e.g. 'M0'
##' @return vec_row dataframe row with reference, and max residual
force_trial_to_stable_metrics <- function(force_trial, last_n_milliseconds, muscle) {
  ref <- tail(force_trial, 1)[, reference(muscle)]
  measured_tail <- tail(force_trial, last_n_milliseconds)[, measured(muscle)]
  max_residual <- max(abs(range(measured_tail) - ref))
  vec_row <- data.frame(last_n_milliseconds = last_n_milliseconds, reference = ref,
    sd = sd(measured_tail), max_residual = max_residual)
  return(vec_row)
}
##' ft_list_to_stability_df_rows
##' @param list_of_force_trials each a force_trial time series
##' @param last_n_milliseconds the number of tail milliseconds from which we should calculate the settled standard deviation.
##' @param muscle the muscle of interest in a string e.g. 'M0'
##' @return stability_df_rows df with reference (desired) muscle tension, variance, maximum residual
ft_list_to_stability_df_rows <- function(list_of_force_trials, last_n_milliseconds,
  muscle) {
  reference_df_rows <- lapply(list_of_force_trials, force_trial_to_stable_metrics,
    last_n_milliseconds, muscle)
  return(do.call("rbind", reference_df_rows))
}

##' @title How many force trials meet the spec
##' @description if the spec is a certain envelope size for error?
##' Produces a plot of remaining fraction as a function of increasing err threshold. Draws a hardcoded line for the 99%
##' @param force_trials a list of raw force timeseries_DFs. has_settled has not been applied yet
##' @param err_lenout integer, the resolution along the X axis by which diff err thresholds will be evaluated
##' @importFrom parallel mclapply
try_different_stability_thresholds <- function(force_trials, err_lenout = 20) {
  different_errors <- seq(0.1, 1, length.out = err_lenout)
  num_remaining_force_trials <- unlist(mclapply(different_errors, function(err) {
    length(remove_unsettled_force_trials(force_trials, err))
  }))
  plot_remaining_force_trial_fraction_as_function_of_err(different_errors, num_remaining_force_trials,
    length(force_trials))
  return(cbind(different_errors, num_remaining_force_trials))
}

##' Indices from a Posture DF row'
##'@param posture_df_row a dataframe with initial and final columns
##'@return indices a list of two elements, the initial and final index.
indices_from_posture_df_row <- function(posture_df_row) c(posture_df_row[["initial"]],
  posture_df_row[["final"]])

##' Compute and save ForceTrials to RDS files'
##' Turn a dataframe of collected observations, with square input to reference, into formal ForceTrial objects
##' Works with stabilized or nonstabilized force trials. It will add attributes of stabilization only for those that meet the err criteria
##' saves each to a folder
##' @param df_row dataframe with 1 row with columns including the initial and final indices in reference to a posture collected.
##' @param column_to_separate_forces i.e. 'measured_M0'
##' @param data_location path to realTimeData2017_08_16_13_23_42.txt
##' @param full_df realTimeData2017_08_16_13_23_42.rds
##' @param err max allowable residual from reference force for muscle of interest
save_ForceTrials_to_rds <- function(df_row, column_to_separate_forces, data_location,
  full_df, err) {
  indices <- indices_from_posture_df_row(df_row)
  force_trial_file_string_with_posture <- paste0("force_trial_adept_x_", df_row[["adept_x"]],
    "_adept_y_", df_row[["adept_y"]], ".rds")
  force_list <- get_forces_list(full_df, indices, column_to_separate_forces)
  print("Coerscing to ForceTrial")
  force_trials_list <- lapply(force_list, ForceTrial, data_location, full_df, err)
  print("Saving RDS to Resilio")
  save_rds_to_Resilio(force_trials_list, force_trial_file_string_with_posture)
  print("Cleaning up memory")
  rm(force_trials_list, force_trials_list)
}

##' Turn a dataframe of collected observations, with square input to reference, into formal ForceTrial objects
##' Works with stabilized or nonstabilized force trials. It will add attributes of stabilization only for those that meet the err criteria
##' saves each to a folder
##' @param df_of_postures dataframe with columns including the initial and final indices in reference to each posture group collected.
##' @param full_df dataframe object of the full dataset collected, from which the indices will extract the sections.
##' @importFrom pbmcapply pbmclapply
posture_start_finish_indices_to_L_of_ForceTrials <- function(df_of_postures, full_df,
  data_location, err) {
  lapply(df_to_list_of_rows(df_of_postures), save_ForceTrials_to_rds, column_to_separate_forces = "measured_M0",
    data_location, full_df, err)
}

##' Posture path to stability data frame
##' @param posturepath string full path to RDS
##' @return the stability df for that postures
posture_path_to_stability_df <- function(posturepath) {
  posture <- readRDS(posturepath)
  max_residuals_and_sd <- lapply(posture, ForceTrial_to_signed_max_residual_and_sd)
  stability <- cbind(ForceTrials_to_stability_info_df(posture), ForceTrials_to_stability_df(posture))
  adept_coords <- adept_coordinates_from_ForceTrial(posture[[1]])
  posture_and_residual_sd <- dcrb(lapply(max_residuals_and_sd, add_posture_to_max_residual_and_sd,
    adept_coords))
  d <- merge(stability, posture_and_residual_sd)
  return(d)
}
##' Get the signed residuals from all postures
##' @param rds_postures list of full filepaths to each rds Posture, each with a list of ForceTrials
##' @return stability_df data.frame with all stabiliyt information observations at postures. Includes adept xy coordinates
get_stability_df_for_all_postures <- function(rds_postures) {
  stability_df <- dcrb(pbmclapply(rds_postures, posture_path_to_stability_df))
  return(stability_df)
}

############ Plotting

##' sd_residual_plot_hex
##' @param stability_df dataframe with adept_x and adept_y column, sd, and signed_max_residual
##' @return p plot object for ggplot scatter plot.
sd_residual_plot_hex <- function(stability_df, size = 0.25) {
  my_breaks = c(1, 10, 100, 1000, 10000, 1e+05)
  ggplot(stability_df, aes(sd, signed_max_residual)) + theme_bw() + stat_binhex(bins = 100) +
    scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks) +
    xlab("sd of last 100ms") + ylab("max(residuals of last 100ms)")
}

##' sd_residual_plot_scatter_posture
##' @param list_of_stability_dfs dataframes with adept_x and adept_y column, sd, and signed_max_residual. first is fix_x_sdres
##' @param adept_dimension_that_changes either 'adept_x' or 'adept_y', describing the one that is changing
##' @return p_list plot object for ggplot scatter plot.
sd_residual_plot_scatter_posture <- function(stability_df, adept_dimension_that_changes,
  size = 0.25) {
  my_breaks = c(1, 10, 100, 1000, 10000, 1e+05)
  p <- ggplot(stability_df, aes(sd, signed_max_residual)) + geom_point(size = size) +
    theme_bw() + xlab(paste("sd of last 100ms,", adept_dimension_that_changes)) +
    ylab("max(residuals of last 100ms)") + theme_bw()
  return(p)
}

##' Adept Boxplots to show how posture affects some y
##' the length of the whiskers =  1.5 the IQR
##' @param stability_df dataframe with adept_* and y variable to respond
##' @param adept_dimension_that_changes either 'adept_x' or 'adept_y', describing the one that is changing
##' @param response_variable e.g. 'sd' or 'signed_max_residual'
##' @return p ggplot plot object
adept_boxplots <- function(stability_df, adept_dimension_that_changes, response_variable) {
  adept_range <- range(stability_df[[adept_dimension_that_changes]])
  min_distance_between_adept_postures <- abs(min(diff(unique(stability_df[[adept_dimension_that_changes]])))) *
    0.9
  p <- ggplot(stability_df, aes_string(y = response_variable, group = adept_dimension_that_changes))
  p <- p + geom_boxplot(aes_string(cut_width(stability_df[[adept_dimension_that_changes]],
    min_distance_between_adept_postures)), outlier.alpha = 0.2, outlier.size = 0.1,
    alpha = 0.7, size = 0.5)
  p <- p + theme_bw() + xlab(paste(adept_dimension_that_changes, "postures from", adept_range[1], "to", adept_range[2]))
  p <- p + theme(panel.background = element_rect(fill = "white", colour = "grey50"))
  p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  return(p)
}

##' Relationship between adept posture and signed max residual in scatter plot form
##' @param stability_df dataframe with adept_x and adept_y column, sd, and signed_max_residual
##' @param adept_dimension_that_changes either 'adept_x' or 'adept_y', describing the one that is changing (the one that's not fixed)
##' @return p ggplot object
signed_max_residual_vs_posture <- function(stability_df, adept_dimension_that_changes) {
  p <- ggplot(stability_df, aes_string(adept_dimension_that_changes, "signed_max_residual"))
  p <- p + geom_point(size = 0.1) + theme_bw()
  return(p)
}

##' Produce stability plots in list
##' @param stability_df dataframe with adept_x and adept_y column, sd, and signed_max_residual
##' @param adept_dimension_that_changes either 'adept_x' or 'adept_y', describing the one that is changing (the one that's not fixed)
##' @return p_list list of plot objects
produce_stability_plots <- function(stability_df, adept_dimension_that_changes) {
  p_sd <- adept_boxplots(stability_df, adept_dimension_that_changes, "sd")
  p_max_residual <- signed_max_residual_vs_posture(stability_df, adept_dimension_that_changes)
  p_sdres_adept <- sd_residual_plot_scatter_posture(stability_df, adept_dimension_that_changes)
  p_sd_residual <- sd_residual_plot_hex(stability_df)
  return(list(p_sd, p_max_residual, p_sdres_adept, p_sd_residual))
}
