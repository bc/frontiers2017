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
  replicate_stable_df <- forcetrials_to_static_response_df_for_replicates(response$dynamic_trials_list,last_n_milliseconds)
  print_latex_table_for_replicate_maps(replicate_stable_df)


  list_of_replicate_results <- split(replicate_stable_df, replicate_stable_df$reference_M0)

  residual_sets_dt <- lapply(list_of_replicate_results, replicate_results_df_to_mean_6dim_magnitude_scatter, force_names_to_predict)
p1 <- residual_from_mean_force_dimension_6dim(residual_sets_dt,force_names_to_predict=force_names_to_predict)
ggsave(to_output_folder("replicate_meanwrench_residual_distributions.pdf"), p1, width=10, height=5, limitsize=FALSE)

  p_magnitudes <- plot_histogram_of_magnitude_residuals(list_of_replicate_results)
  ggsave(to_output_folder("replicate_meanFxyz_magnitude_residuals.pdf"), p_magnitudes, width=3, height=3, limitsize=FALSE)


})
