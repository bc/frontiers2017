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
group_indices <- list(lower = 2, upper = 301)
# Define maps of interest to extract into lists
maps_of_interest <- sections_of_interest$parallel[, muscle_names()]
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
  tails <- extract_tails_from_trials(response[[1]], last_n_milliseconds)
  static_response <- as.data.frame(dcrb(lapply(tails, colMeans))[1:32, ])
  static_response <- data.table(static_response[order(static_response$reference_M0),])
  a <- static_response[, , by = reference_M0]
  # n = 5 replicates per map
  list_of_replicate_results <- lapply(split(static_response, static_response$reference_M0),
    tail, 5)
  residual_sets_dt <- lapply(list_of_replicate_results, function(replicate_results) {
    section <- replicate_results[, force_names_to_predict, with = FALSE]
    residuals_from_mean <- dcrb(lapply(df_to_list_of_rows(section), function(x) {
      as.vector(x) - as.vector(colMeans(section))
    }))
    return(residuals_from_mean)
  })
  names(residual_sets_dt) <- 0:4
  residual_melt <- melt(residual_sets_dt)
  colnames(residual_melt) <- c("force_dimension", "residual_from_mean", "map")
  bin_setting_forces <- 0.2
  bin_setting_torques <- 0.005
   # + scale_x_continuous(limits=c(-0.6,0.6), breaks=seq(-0.2,0.2, by = 0.01))
  p1 <- ggplot(residual_melt[residual_melt$force_dimension %in% force_names_to_predict[1:3],]) + geom_histogram(aes(residual_from_mean, fill=map), binwidth=bin_setting_forces) + facet_wrap(~force_dimension) + theme_minimal() + xlab("Residual from mean force value (N)") + ylab("Count") + scale_x_continuous(breaks = pretty(seq(-0.6, 0.6, by = 0.2))) + xlim(-0.6,0.6)
  p2 <- ggplot(residual_melt[residual_melt$force_dimension %in% force_names_to_predict[4:6],]) + geom_histogram(aes(residual_from_mean, fill=map), binwidth=bin_setting_torques) + facet_wrap(~force_dimension) + theme_minimal()+ xlab("Residual from mean torque value (Nm)") + ylab("Count") + xlim(-0.02,0.02)
  p_residual_sets <- arrangeGrob(p1,p2,nrow=2)
  ggsave(to_output_folder("replicate_residuals.pdf"), p_residual_sets, width=10, height=5, limitsize=FALSE)
  print_latex_table_for_replicate_maps(static_response)

  browser()
  list_of_magnitude_residual_dfs <- list_of_replicates_to_replicates_to_residuals_of_mean_magnitude(list_of_replicate_results)
  norm_vec_residuals_df_melt <- melt(list_of_magnitude_residual_dfs)
  colnames(norm_vec_residuals_df_melt) <- c("index_within_replicates", "residual_from_mean_magnitude", "map")
  norm_vec_residuals_df_melt$index_within_replicates <- NULL
  norm_vec_residuals_df_melt$residual_from_mean_magnitude <- NULL
  colnames(norm_vec_residuals_df_melt) <- c("residual_from_mean_magnitude", "map")

  plot_histogram_of_magnitude_residuals()

})
