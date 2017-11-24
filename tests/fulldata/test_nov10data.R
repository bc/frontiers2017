context("test_nov10data.r")

# Experiment Procedure:

# no_spaces_noise_lo_0_hi_20_nmaps_500_replicates_1.csv --> Hand -->
# noiseTrial2017_11_19_19_45_01.txt noiseTrial2017_11_19_19_45_01.txt --> R -->
# A_fit_har_5_tasks -->
# scaling_task_n100_per_outputvec_of_interest_5_steps_no_replicates.csv
# scaling_task_n100_per_outputvec_of_interest_5_steps_no_replicates.csv --> Hand
# --> scalingResponse.csv

set.seed(4)
last_n_milliseconds <- 100
# Response to noise through hand noiseTrial2017_11_19_19_45_01.txt was on MIT
# hand. noise input 100 = num_maps input:
# no_spaces_noise_lo_0_hi_20_nmaps_500_replicates_1.csv
# noise_response <- as.data.frame(fread(get_Resilio_filepath("noiseTrial2017_11_19_19_53_10.txt")))
noise_response <- as.data.frame(fread(get_Resilio_filepath("noiseResponse2017_11_22_16_01_36_on_mit_hand_500maps_1replicate.txt")))
JR3_sensor_null <- colMeans(head(noise_response, 100))
noise_response <- zero_out_JR3_sensors(noise_response, JR3_sensor_null)
# make sure the JR3 signals respond in some way to the changes.
noise_response_wo_null <- noise_response[noise_response$map_creation_id != 0, ]
p <- plot_measured_command_reference_over_time(noise_response_wo_null)
ggsave("../../output/xray_for_noiseReponse.pdf", p, width=90, height=30, limitsize=FALSE)
# Remove pre-experiment and post experiment stuff
noise_hand_responses_raw <- split_by_map_creation_id(unique(noise_response_wo_null$map_creation_id),
  noise_response)
are_correct_length <- dcc(lapply(noise_hand_responses_raw, function(dt) {
  return(nrow(dt) >= 700 && nrow(dt) < 810)
}))
noise_hand_responses <- noise_hand_responses_raw[are_correct_length]
message(sprintf("Out of the %s collected maps, %s had between 700 and 810 samples. Using %s maps.",
  length(noise_hand_responses_raw), length(noise_hand_responses), length(noise_hand_responses)))
input_output_data <- df_of_hand_response_input_output(noise_hand_responses, last_n_milliseconds)
data <- df_split_into_training_and_testing(input_output_data, fraction_training = 0.8)
training_data <- data$train
test_data <- data$test

force_names_to_predict <- c("JR3_FX", "JR3_FY", "JR3_FZ")
muscles_of_interest <- muscle_names()[1:7]
num_muscles <- length(muscles_of_interest)
A_fit <- find_A_matrix_without_offset(as.data.frame(training_data), measured(muscles_of_interest),
  force_names_to_predict)
fit_evaluation_without_offset(A_fit, as.data.frame(test_data))

range_tension <- c(0, 20)
muscle_constraints_matrix <- diag(rep(1, num_muscles))
generator_columns_A_matrix <- t(A_fit$AMatrix)
dim(generator_columns_A_matrix)
dim(muscle_constraints_matrix)
# Here, identify a force vector of interest and apply it to the generated A
# Matrix. Then permute it to create a task line
ffs_vertex <- generator_columns_A_matrix %*% c(rep(20, 7))  #this is used to get one viable force for the model.
# task_force <- c(-0.5641187, 0, 1)
task_force <- ffs_vertex

############ MANUAL: IDENTIFY TASK MULTIPLIER BOUNDS
pdf("histogram_by_muscle_projections_over_5_tasks.pdf", width = 100, height = 100)
par(mfrow = c(nrow(task_df), num_muscles))



task_multiplier_bounds <- c(0.2, 0.9)  #if c(0.0,0.0) then a null task.
task_multiplier_list <- seq(task_multiplier_bounds[1], task_multiplier_bounds[2],
  length.out = 5)
task_df <- t(task_force %*% t(task_multiplier_list))
colnames(task_df) <- force_names_to_predict
num_samples_desired <- 100
sset <- lapply(df_to_list_of_rows(task_df), function(task_i) {
  constraints_to_points(muscle_column_generators = t(A_fit$AMatrix), range_tension,
    task_i, num_samples_desired, thin = 100)
})
for (i in seq(1, length(sset))) {
  samples <- sset[[i]]
  task <- task_df[i, ]
  predicted_forces <- t(t(A_fit$AMatrix) %*% t(samples))
  points3d(predicted_forces)
  euclidian_errors_vector <- apply(predicted_forces, 1, function(row) norm_vec(row -
    task))
  print(task)
  expect_equal(max(euclidian_errors_vector), 0, tol = 1e-13)
  context("expect no residual from target endpoint force with the produced points")
  # expect_equal(sum(apply(predicted_forces, 2, sd)), 0, tol = 1e-05)
  fas_histogram(samples, range_tension, task, breaks = 50, col = "black", cex = 0.25)
  show_l1_costs(samples, task)
}
dev.off()
##############

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
extract_3cols <- lapply(sset, function(x) x[, c(1, 2, 3)])
gradient <- colorRampPalette(c("#a6cee3", "#1f78b4", "#b2df8a", "#fc8d62", "#ffffb3",
  "#bebada"))
list_of_mats <- add_gradient_to_attrs(extract_3cols, gradient(length(extract_3cols)))

rgl.clear()
axes_for_multiple_sets(list_of_mats)
axes_for_defined_xyz_limits(rep(list(c(0, 20)), 3))
rgl_convhulls(list_of_mats, points = TRUE)
# Add x, y, and z Axes

res <- lapply(sset, create_and_cbind_map_creation_ids, muscle_names())
big_har_set_to_test_on_finger <- dcrb(res)
write.csv(big_har_set_to_test_on_finger, "../../output/scaling_task_n100_per_outputvec_of_interest_5_steps_no_replicates.csv",
  row.names = FALSE, quote = FALSE)
# Make sure the output looks correct
prescribed_maps <- as.data.frame(fread("../../output/scaling_task_n100_per_outputvec_of_interest_5_steps_no_replicates.csv"))
maps_without_ids <- unique(prescribed_maps[muscle_names()])
expected_forces <- t(as.matrix(A_fit$AMatrix)) %*% t(as.matrix(maps_without_ids[,
  muscles_of_interest]))
  # , xlim = c(-0.5, 0), ylim = c(-2, 2), zlim = c(-2, 2)
plot3d(t(expected_forces))
## QUickly make sure that the saved file has only 5 output points expected.

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
noise_hand_responses <- scaling_hand_responses_raw[are_correct_length]
message(sprintf("Out of the %s collected maps, %s had between 700 and 810 samples. Using %s maps.",
  length(noise_hand_responses_raw), length(noise_hand_responses), length(noise_hand_responses)))


maps_with_ids <- dcrb(lapply(noise_hand_responses, tail, 1))
maps_without_ids <- unique(sample_maps_data_scaling_wo_null[reference(muscle_names())])
expected_forces <- t(as.matrix(A_fit$AMatrix)) %*% t(as.matrix(maps_without_ids[,
  reference(muscles_of_interest)]))
plot3d(t(expected_forces), xlim = c(-0.5, 0), ylim = c(-2, 2), zlim = c(-2, 2))
