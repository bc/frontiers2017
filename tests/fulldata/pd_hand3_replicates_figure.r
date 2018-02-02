context("evaluating_replicates")


sections_of_interest <- dec20_PD_EXTMECH_maps_of_interest_by_section()
signals_prefix <- "get_null_indices_via_this_plot_of_untransformed_xray_for_"

# Define dec20 hand3 parameters
JR3_to_fingertip_distance <- 0.00802  #about 9mm in meters TODO
last_n_milliseconds <- 100
range_tension <- c(0, 10)
muscles_of_interest <- muscle_names()
num_muscles <- length(muscles_of_interest)
force_names_to_predict <- c("JR3_FX", "JR3_FY", "JR3_FZ", "JR3_MX", "JR3_MY", "JR3_MZ")
# Define maps of interest to extract into lists
maps_of_interest <- sections_of_interest$replicates[, muscle_names()]
colnames(maps_of_interest) <- reference(muscle_names())

lapply_to_dfs <- function(list_of_non_dataframes){lapply(list_of_non_dataframes, as.data.frame)}
test_that("hand 3 ultraflex REPLICATES", {

  filename_3A <- "noiseResponse_ST1BC_2017_12_20_19_50_38_PD_Extmech_good_ultraflex_NOTAP.txt"
  hand3_dec20_ultraflex <- fread_df_from_Resilio(filename_3A)
  manual_3tap_for_hand3_ultraflex <- fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_20_20_05_37_manual_3tap_extmech_for_ultraflex.txt")
  JR3_sensor_null <- colMeans(manual_3tap_for_hand3_ultraflex[15000:20000, ])
  noise_response_wo_null <- munge_JR3_data(hand3_dec20_ultraflex, remove_nonzero_map_creation_ids = FALSE,
    input_are_voltages = TRUE, JR3_to_fingertip_distance = JR3_to_fingertip_distance,
    JR3_sensor_null = JR3_sensor_null)
  response <- extract_static_and_dynamic_data(noise_response_wo_null, group_indices = list(lower = 302,
    upper = 352), last_n_milliseconds = 100)
  replicate_stable_df <- forcetrials_to_static_response_df_for_replicates(response$dynamic_trials,last_n_milliseconds)

  # n = 5 replicates per map. We wanted to get a consistent sample size for each of the replicate trials.
  list_of_replicate_results <- split(replicate_stable_df, replicate_stable_df$reference_M0)
  residual_sets_dt <- lapply(list_of_replicate_results, function(replicate_results) {
    section <- replicate_results[, force_names_to_predict, with = FALSE]
    residuals_from_mean <- dcrb(lapply(df_to_list_of_rows(section), function(x) {
      as.vector(x) - as.vector(colMeans(section))
    }))
    return(residuals_from_mean)
  })
  names(residual_sets_dt) <- 0:4

  print_how_many_samples_of_each_map_were_collected(residual_sets_dt)

  lapply(residual_sets_dt, function(replicate_rows_df){
    replicate_rows_df$n_replicate_sample_notes <-
  })
  residual_melt <- melt(residual_sets_dt)
  colnames(residual_melt) <- c("force_dimension", "residual_from_mean", "map")
  bin_setting_forces <- 0.2
  bin_setting_torques <- 0.005

   split_wrench <- halve_force_dimension_value_df_into_forces_and_torques(residual_melt)

  p1 <- arrangeGrob(plot_boxplot_faceted_by_JR3(split_wrench$forces),
                                 plot_boxplot_faceted_by_JR3(split_wrench$torques),nrow=2)
  ggsave(to_output_folder("replicate_residuals.pdf"), p1, width=10, height=5, limitsize=FALSE)
  print_latex_table_for_replicate_maps(static_response)

  browser()
  list_of_magnitude_residual_dfs <- list_of_replicates_to_replicates_to_residuals_of_mean_magnitude(list_of_replicate_results)
  norm_vec_residuals_df_melt <- melt(list_of_magnitude_residual_dfs)
  colnames(norm_vec_residuals_df_melt) <- c("index_within_replicates", "residual_from_mean_magnitude", "map")
  norm_vec_residuals_df_melt$index_within_replicates <- NULL
  norm_vec_residuals_df_melt$residual_from_mean_magnitude <- NULL
  colnames(norm_vec_residuals_df_melt) <- c("residual_from_mean_magnitude", "map")
  ggplot(norm_vec_residuals_df_melt) + geom_histogram(aes(residual_from_mean_magnitude  ))

  plot_histogram_of_magnitude_residuals()

})
