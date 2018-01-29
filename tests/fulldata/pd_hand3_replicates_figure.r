context("evaluating_replicates")

#### Helper functions
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
  residuals_per_map <- as.data.frame(dcrb(lapply(list_of_replicate_results, function(replicate_results) {
    section <- replicate_results[, force_names_to_predict, with = FALSE]
    residuals_from_mean <- dcrb(lapply(df_to_list_of_rows(section), function(x) {
      as.vector(x) - as.vector(colMeans(section))
    }))
    return(residuals_from_mean)
  })))
  pdf(to_output_folder("histogram_per_absolute_residual_from_vector_dimension.pdf"), width=10, height = 10)
  histogram_per_absolute_residual_from_vector_dimension(residuals_per_map)
  dev.off()
  print_latex_table_for_replicate_maps(static_response)

})
