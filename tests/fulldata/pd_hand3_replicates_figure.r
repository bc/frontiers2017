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


test_that('rmse and mse work', {
  expect_equal(rmse(c(1,1,1),c(1,1,2)), 0.5773503, 1e-5)
  expect_equal(rmse(c(1,1,1),c(1,1,1)), 0, 1e-5)
  expect_equal(mse(c(1,1,1),c(1,1,1)), 0, 1e-5)
  expect_equal(mse(c(1,1,2),c(1,1,1)), 0.33333, 1e-5)
})

test_that("hand 3 ultraflex REPLICATES", {
  filename_3A <- "noiseResponse_ST1BC_2017_12_20_19_50_38_PD_Extmech_good_ultraflex_NOTAP.txt"
  hand3_dec20_ultraflex <- fread_df_from_Resilio(filename_3A)
  manual_3tap_for_hand3_ultraflex <- fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_20_20_05_37_manual_3tap_extmech_for_ultraflex.txt")
  JR3_sensor_null <- colMeans(manual_3tap_for_hand3_ultraflex[15000:20000,])
  noise_response_wo_null <- munge_JR3_data(hand3_dec20_ultraflex, remove_nonzero_map_creation_ids = FALSE,
    input_are_voltages = TRUE, JR3_to_fingertip_distance = JR3_to_fingertip_distance,
    JR3_sensor_null = JR3_sensor_null)
      list_of_replicate_results <- lapply(replicate_df_list_from_noise_response(noise_response_wo_null, last_n_milliseconds), tail,5)
  residual_sets_dt <- lapply(list_of_replicate_results, replicate_results_df_to_mean_6dim_magnitude_scatter, force_names_to_predict)
  p1 <- residual_from_mean_force_dimension_6dim(residual_sets_dt)
  ggsave(to_output_folder("replicate_meanwrench_residual_distributions.pdf"), p1, width=10, height=5, limitsize=FALSE)
  p_magnitudes <- plot_histogram_of_magnitude_residuals(list_of_replicate_results)
  ggsave(to_output_folder("replicate_meanFxyz_magnitude_residuals.pdf"), p_magnitudes, width=3, height=3, limitsize=FALSE)
 mean_euclidian_residuals_for_replicates<- euclidian_residuals_for_replicates_from_noise_response(noise_response_wo_null, 100,5)
message("for n=5 maps, what is the mean euclidian residual (in N) from each map's mean wrench value, num replicates = fixed to 5:")
message(mean(mean_euclidian_residuals_for_replicates))
browser()
})
