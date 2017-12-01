context("test_nov10data.r")

# Experiment Procedure:

# no_spaces_noise_lo_0_hi_20_nmaps_500_replicates_1.csv --> Hand -->
# noiseTrial2017_11_19_19_45_01.txt noiseTrial2017_11_19_19_45_01.txt --> R -->
# A_fit_har_5_tasks -->
# scaling_task_n100_per_outputvec_of_interest_5_steps_no_replicates.csv
# scaling_task_n100_per_outputvec_of_interest_5_steps_no_replicates.csv --> Hand
# --> scalingResponse.csv

set.seed(4)
#######PARAMS##############
last_n_milliseconds <- 100
range_tension <- c(0, 20)
muscles_of_interest <- muscle_names()[1:7]
num_muscles <- length(muscles_of_interest)
force_names_to_predict <- c("JR3_FX","JR3_FY","JR3_FZ","JR3_MX","JR3_MY","JR3_MZ")
#######PARAMS##############

# Response to noise through hand noiseTrial2017_11_19_19_45_01.txt was on MIT
# hand. noise input 100 = num_maps input:
# no_spaces_noise_lo_0_hi_20_nmaps_500_replicates_1.csv

untransformed_noise_response <- as.data.frame(fread(get_Resilio_filepath("noiseResponse2017_11_30_20_16_06_500_maps_reps_1.txt")))
noise_response_wo_null <- munge_JR3_data(untransformed_noise_response, input_are_voltages=TRUE, indices_for_null=50:250)
p <- plot_measured_command_reference_over_time(noise_response_wo_null)
ggsave(to_output_folder("xray_for_noiseReponse.pdf"), p, width=90, height=30, limitsize=FALSE)
noise_hand_responses <- split_by_map_and_remove_wrongly_lengthed_samples(noise_response_wo_null)
input_output_data <- df_of_hand_response_input_output(noise_hand_responses, last_n_milliseconds)
A_fit <- A_fit_from_80_20_split(input_output_data, muscles_of_interest, force_names_to_predict)
generator_columns_A_matrix <- t(t(A_fit$AMatrix) %*% diag(7))
compute_ranks_of_A(A_fit$AMatrix)
# Here, identify a force vector of interest and apply it to the generated A
# ffs_vertex <- generator_columns_A_matrix %*% c(rep(20, 7))  #this is used to get one viable force for the model.
# ffs_vertex2 <- generator_columns_A_matrix %*% c(c(10,5,15,10,5,10,20))  #this is used to get one viable force for the model.
binary_combinations <- custom_binary_combinations(7,c(0,20))
binary_combination_ffs_points <- binary_combinations %*% generator_columns_A_matrix
plot_ffs_with_vertices(binary_combination_ffs_points[,1:3], generator_columns_A_matrix[,1:3], alpha_transparency=0.25, range_tension=range_tension)
points3d(input_output_data[,force_names_to_predict][,1:3], size=1, col="black", alpha=1)
title3d(main="FFS", xlab="Fx", ylab="Fy", zlab="Fz", col="black")


message('Pick 2 points to define the horizontal line endpoints.')

horizontal_line_points <- identify_n_points_from_pointcloud(binary_combination_ffs_points[,1:3],n=2)
horizontal_line_tasks <- dcrb(draw_perpendicular_line(horizontal_line_points[1,],horizontal_line_points[2,],5))
spheres3d(horizontal_line_tasks, r=0.10, col="pink")
message('Pick 1 point to define the scaling direction.')
ffs_vertex_rep20 <- t(generator_columns_A_matrix) %*% as.matrix(rep(20, 7))
task_direction_to_scale <- identify_n_points_from_pointcloud(binary_combination_ffs_points[,1:3],n=1)
map_that_created_task_dir <- binary_combinations[which(binary_combination_ffs_points[,1]==task_direction_to_scale[1]),]
#Now show the torque FAS
rgl.init()
plot_ffs_with_vertices(binary_combination_ffs_points[,4:6], generator_columns_A_matrix[,4:6], alpha_transparency=0.25, range_tension=range_tension)



############ MANUAL: IDENTIFY TASK MULTIPLIER BOUNDS FOR SCALING
task_multiplier_bounds <- c(0, 1.0)
task_multiplier_list <- seq(task_multiplier_bounds[1], task_multiplier_bounds[2],
  length.out = 5)
task_df <- t(task_direction_to_scale %*% t(task_multiplier_list))
colnames(task_df) <- force_names_to_predict[1:3]
num_samples_desired <- 1000
sset <- lapply(df_to_list_of_rows(task_df), function(task_i) {
  constraints_inc_torque_to_points(muscle_column_generators = t(A_fit$AMatrix), range_tension,
    task_i, num_samples_desired, thin = 100,torque_max_deviation=5.0)
})

pdf(to_output_folder("histogram_by_muscle_projections_over_5_tasks.pdf"), width = 100, height = 100)
  par(mfrow = c(nrow(task_df), num_muscles))
  lapply(seq(1, length(sset)), function(i){
    fas_histogram(sset[[i]], range_tension, task_df[i, ], breaks = 50, col = "black", cex = 0.25)
  })
dev.off()
 ##TODO turn this into a funciton that takes in the sset list and plots the
 ## sset_eval_if_reasonable => T or F whether the vals are good.

for (i in seq(1, length(sset))) {
  samples <- sset[[i]]
  task <- task_df[i, ]
  predicted_forces <- t(t(A_fit$AMatrix) %*% t(samples))
  euclidian_errors_vector <- apply(predicted_forces, 1, function(row) norm_vec(row -
    task))
    maps_match_desired <- max(abs(euclidian_errors_vector)) < 10
    message(task)
    message(paste("Good?", maps_match_desired))

  if(maps_match_desired){
  show_l1_costs(samples, task)
  spheres3d(predicted_forces, r=0.1, col="green")}
}

####### Expect to see five distinct points. one for each task.
rgl.init()
list_of_predicted_forces <- lapply(sset, function(samples) {
  t(t(A_fit$AMatrix) %*% t(samples))
})
plot3d(dcrb(list_of_predicted_forces))
#######


####### Visualize the FAS sets that are produced by feeding in the noiseResponse data.
rgl.init()
num_tasks <- length(sset)
rgl_init(bg = "white")
extract_3cols <- lapply(sset, function(x) x[, c(2, 3, 4)])
gradient <- colorRampPalette(c("#a6cee3", "#1f78b4", "#b2df8a", "#fc8d62", "#ffffb3",
  "#bebada"))
list_of_mats <- add_gradient_to_attrs(extract_3cols, gradient(length(extract_3cols)))
list_of_mats <- lapply(list_of_mats, function(x) x/1e5)
rgl.clear()
axes_for_multiple_sets(list_of_mats)
axes_for_defined_xyz_limits(rep(list(c(0, 20)), 3))
rgl_convhulls(list_of_mats, points = TRUE)

res <- lapply(sset, create_and_cbind_map_creation_ids, muscle_names())
big_har_set_to_test_on_finger <- dcrb(res)
write.csv(big_har_set_to_test_on_finger, to_output_folder("scaling_task_n100_per_outputvec_of_interest_5_steps_no_replicates.csv"),
  row.names = FALSE, quote = FALSE)
# Make sure the output looks correct
prescribed_maps <- as.data.frame(fread(to_output_folder("scaling_task_n100_per_outputvec_of_interest_5_steps_no_replicates.csv")))
maps_without_ids <- unique(prescribed_maps[muscle_names()])
expected_forces <- t(as.matrix(A_fit$AMatrix)) %*% t(as.matrix(maps_without_ids[,
  muscles_of_interest]))
  # , xlim = c(-0.5, 0), ylim = c(-2, 2), zlim = c(-2, 2)
plot3d(t(expected_forces))
## Quickly make sure that the saved file has only 5 output points expected.

## TODO GET data from the cadaver finger from big_har_set_to_test_on_finger this
## is the response when you push in the maps for 5 tasks through the finger

# noiseResponse2017_11_22_16_24_55.txt
# response_to_prescribed_har_maps <- as.data.frame(fread(get_Resilio_filepath("noiseTrial2017_11_19_20_21_33.txt")))
response_to_prescribed_har_maps <- as.data.frame(fread(get_Resilio_filepath("noiseResponse2017_11_22_16_31_59_response_to_maps_5_tasks.txt")))
JR3_sensor_null_for_prescribed_har_maps <- colMeans(head(response_to_prescribed_har_maps,
  30))
sample_maps_data_scaling <- zero_out_JR3_sensors(response_to_prescribed_har_maps,
  JR3_sensor_null_for_prescribed_har_maps)
p <- ggplot(data = head(sample_maps_data_scaling, 30))
p <- p + geom_line(aes(time, JR3_FX))
p <- p + geom_line(aes(time, JR3_FY))
p <- p + geom_line(aes(time, JR3_FZ))
p
### TODO Compare jr3 nulls from response_to_noise and
### response_to_prescribed_har_maps to see if drift happened


# Remove pre-experiment and post experiment stuff
sample_maps_data_scaling_wo_null <- sample_maps_data_scaling[sample_maps_data_scaling$map_creation_id !=
  0, ]

  plot_input_output_signals(head(sample_maps_data_scaling_wo_null, 10000))
  plot_input_output_signals(head(sample_maps_data_scaling_wo_null, 10000), command)
  plot_input_output_signals(head(sample_maps_data_scaling_wo_null, 10000), reference)
  plot_input_output_signals(downsampled_df(sample_maps_data_scaling_wo_null, 100))
  plot_input_output_signals(downsampled_df(sample_maps_data_scaling_wo_null, 100), reference)
  plot_input_output_signals(downsampled_df(sample_maps_data_scaling_wo_null, 100), command)
  # make sure the JR3 signals respond in some way to the changes.

# Expect to see only force trials with none of the warmup. ending forcetrial will
# be cut off
plot_input_output_signals(head(sample_maps_data_scaling_wo_null, 10000))
# Expect to see only force trials with none of the cooldown. Beginning ft will be
# cut off
plot_input_output_signals(tail(sample_maps_data_scaling_wo_null, 10000))


scaling_hand_responses_raw <- split_by_map_creation_id(unique(sample_maps_data_scaling_wo_null$map_creation_id),
  sample_maps_data_scaling_wo_null)
are_correct_length <- dcc(lapply(scaling_hand_responses_raw, function(dt) {
  return(nrow(dt) >= 700 && nrow(dt) < 810)
}))
scaling_hand_responses <- scaling_hand_responses_raw[are_correct_length]
message(sprintf("Out of the %s collected maps, %s had between 700 and 810 samples. Using %s maps.",
  length(scaling_hand_responses_raw), length(scaling_hand_responses), length(scaling_hand_responses)))

scaling_input_output_data <- as.data.frame(df_of_hand_response_input_output(scaling_hand_responses, last_n_milliseconds))

expected_forces <- t(as.matrix(A_fit$AMatrix)) %*% t(as.matrix(scaling_input_output_data[,
  reference(muscles_of_interest)]))

  experimental_forces <- scaling_input_output_data[force_names_to_predict]
  residuals_in_3d <- experimental_forces - t(expected_forces)
  residual_euclidian_magnitudes <- apply(residuals_in_3d, 1, norm_vec)


plot3d(t(expected_forces))
plot3d(scaling_input_output_data[force_names_to_predict])
