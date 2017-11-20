context("test_nov10data.r")
sample_maps_data <- as.data.frame(fread(get_Resilio_filepath('noiseTrial2017_11_19_16_45_54.txt')))
JR3_sensor_null <- colMeans(head(sample_maps_data, 100))
sample_maps_data <- zero_out_JR3_sensors(sample_maps_data, JR3_sensor_null)
plot_input_output_signals(head(sample_maps_data, 10000))
plot_input_output_signals(head(sample_maps_data, 10000), command)
plot_input_output_signals(head(sample_maps_data, 10000), reference)
plot_input_output_signals(downsampled_df(sample_maps_data,1000))
plot_input_output_signals(downsampled_df(sample_maps_data,100), reference)
plot_input_output_signals(downsampled_df(sample_maps_data,100), command)
#make sure the JR3 signals respond in some way to the changes.
plot_input_output_signals(downsampled_df(sample_maps_data,100), reference)
sample_maps_data_wo_null <- sample_maps_data[sample_maps_data$map_creation_id!=0.0,]
# Remove pre-experiment and post experiment stuff
dts <- split_by_map_creation_id(unique(sample_maps_data_wo_null$map_creation_id), sample_maps_data)

df_of_concatenated_replicates <- dts[[1]]
replicate_list <- lapply(dts, split_by_replicate)
sd_of_replicates <- dcrb(lapply(replicate_list, column_sd_across_replicates))
boxplot(sd_of_replicates[,measured(muscle_names())], xlab="Muscle", ylab="SD (N)")
boxplot(sd_of_replicates[,command(muscle_names())], xlab="Muscle", ylab="SD voltage?")
boxplot(sd_of_replicates[,dots_to_underscores(force_column_names)], xlab="Muscle", ylab="SD in Force (N)")

#only take first replicate for each map_creation_id
raw_noise_trials <- unlist(lapply(replicate_list, head,1), recursive=FALSE)

are_correct_length <- dcc(lapply(raw_noise_trials, function(dt) {
  return(nrow(dt) >= 700 && nrow(dt) < 805)
}))
# maps <- dts #TODO later, split off the replicates

noise_trials <- raw_noise_trials[are_correct_length]
input_output_data <- dcrb(lapply(lapply(noise_trials, tail, 100), colMeans))

data <- df_split_into_training_and_testing(input_output_data, fraction_training = 0.8)
training_data <- data$train
test_data <- data$test

force_names_to_predict <- c("JR3_FX", "JR3_FY", "JR3_FZ")
num_muscles <- 7
A_fit <- find_A_matrix_without_offset(as.data.frame(training_data), reference(muscle_names()),
  force_names_to_predict)
fit_evaluation_without_offset(A_fit, as.data.frame(test_data))

range_tension <- c(0.0, 20.0)
muscle_constraints_matrix <- diag(rep(1, num_muscles))
generator_columns_A_matrix <- t(A_fit$AMatrix)
dim(generator_columns_A_matrix)
dim(muscle_constraints_matrix)
browser()
generator_columns_A_matrix %*% c(rep(20,7)) #this is used to get one viable force for the model.
task_force <- c(-0.5641187,  0.4335497, 0.6873463)
task_multiplier_bounds <- c(0.0, 1.0) #if c(0.0,0.0) then a null task.
task_multiplier_list <- seq(task_multiplier_bounds[1], task_multiplier_bounds[2],
  length.out = 5)
task_df <- t(task_force %*% t(task_multiplier_list))
colnames(task_df) <- force_names_to_predict

sset <- lapply(df_to_list_of_rows(task_df), function(task_i) {
  constr <- task_and_generators_to_constr(generator_columns_A_matrix, muscle_constraints_matrix,
    range_tension, task_i)
  constraints_are_feasible(constr)
  state <- har.init(constr, thin = 100)
  result <- har.run(state, n.samples = 100)
  samples <- result$samples
  # Try running those samples back through the A_matrix
  predicted_forces <- predict_output_force(A_fit$AMatrix,samples)
  maximums <- apply(predicted_forces,2,max)
  minimums <- apply(predicted_forces,2,min)
  expect_equal(norm_vec(maximums- minimums), 0, tol=1e-10)
  # Show histograms of the FAS
  par(mfrow = c(1, 7))
  lapply(1:7, function(muscle_num) {
    hist(samples[, muscle_num], breaks = 10, main = paste("M", muscle_num, "at",
      format(task_i, digits = 3), collapse = ""), xlab = "Tendon force (N)",
      xlim = c(0, 11))
  })


##' @param df a dataframe where each row is a nrow(df)- dimensional vector
##' @param v a vector of
  lowest_l1_cost_soln <- function(df) df[which.min(rowSums(df)), ]
  highest_l1_cost_soln <- function(df) df[which.max(rowSums(df)), ]
  message(paste("TASK:",task_i))
  message("-------------------------")
  message("Lowest l1 cost solution:")
  message(format(lowest_l1_cost_soln(samples), digits = 2))
  message("Highest l1 cost solution:")
  message(format(highest_l1_cost_soln(samples), digits = 2))
  message("===========================")

  test_predicted_response <- as.matrix(samples %*% A_fit$AMatrix)
  boxplot(test_predicted_response, ylab = "Tension N for FX,FY,FZ, Torque Nm for MX,MY,MZ",
    main = "what do most of the FAS-sampled forces product in output space? ")
  plot3d(test_predicted_response)
  return(samples)
})

rgl.clear()


# TODO get the test data from the actual data collected test_observed_response <-
# test_data[force_column_names] res_test <- test_observed_response -
# test_predicted_response summary(res_test)

num_tasks <- length(sset)
rgl_init(bg = "white")
extract_3cols <- lapply(sset, function(x) x[,c(1,2,3)])
gradient <- colorRampPalette(c("#a6cee3", "#1f78b4", "#b2df8a", "#fc8d62", "#ffffb3",
  "#bebada"))
list_of_mats <- add_gradient_to_attrs(extract_3cols, gradient(length(extract_3cols)))

rgl.clear()
axes_for_multiple_sets(list_of_mats)
axes_for_defined_xyz_limits(rep(list(c(0,20)),3))
rgl_convhulls(list_of_mats, points=TRUE)
# Add x, y, and z Axes


res <- lapply(sset, function(samples) {
  cbound <- cbind(generate_map_creation_ids(nrow(samples)), as.data.frame(samples))
  colnames(cbound) <- c("map_creation_id", muscle_names())
  return(cbound)
})

big_har_set_to_test_on_finger <- dcrb(res)

write.csv(big_har_set_to_test_on_finger, "scaling_task_n100_per_outputvec_of_interest_5_steps_no_replicates.csv",
  row.names = FALSE, quote = FALSE)
# make a little db to remember which map was trying to achieve which task.
tasklists <- lapply(seq(0.9, 2.8, length.out = 10), function(x) {
  dcrb(rep(list(x * c(0.33152926, 0.07102741, 0.22046813)), 100))
})
task_list_df <- dcrb(tasklists)
colnames(task_list_df) <- force_names_to_predict
maps_with_target_tasks <- cbind(big_har_set_to_test_on_finger, task_list_df)

## TODO GET data from the cadaver finger from big_har_set_to_test_on_finger
###################################################################################################
scaling <- as.data.frame(fread(get_Resilio_filepath("noiseTrial2017_11_18_15_05_33.txt")))

JR3_sensor_null <- colMeans(head(scaling, 30))
# TODO test zero out sensors
sample_maps_data_scaling <- zero_out_JR3_sensors(scaling, JR3_sensor_null)
p <- ggplot(data = head(sample_maps_data_scaling, 30))
p <- p + geom_line(aes(time, JR3_FX))
p <- p + geom_line(aes(time, JR3_FY))
p <- p + geom_line(aes(time, JR3_FZ))
p

dts_scaling <- split(sample_maps_data_scaling, sample_maps_data_scaling$map_creation_id)
are_correct_length <- dcc(lapply(dts_scaling, function(dt) {
  return(nrow(dt) >= 700 && nrow(dt) < 805)
}))


maps_scaling <- dts[are_correct_length]
scaling_input_output_data <- as.data.frame(dcrb(lapply(lapply(maps, tail, 100), colMeans)))
maps_with_target_tasks$map_creation_id <- as.numeric(as.character(maps_with_target_tasks$map_creation_id))
a <- merge(scaling_input_output_data, maps_with_target_tasks, by = "map_creation_id")



## later, do some more stuff with replicates

# noPostureNeutralForceTrials2017_11_18_12_22_04.txt
evaluate_if_length_of_ForceTrials_is_appropriate <- function(raw_df){
  #trim off nullm
 ft_lengths <- a[map_creation_id!=0][,.N,'map_creation_id'][,N]
 a_without_pre_experiment <- a[map_creation_id!=0][,,'map_creation_id']
 b <- split(a_without_pre_experiment,a_without_pre_experiment$map_creation_id)
 ft_start_times <- dcc(lapply(b, function(df) head(df$time,1)))
 means <- list_of_mean_of_last_n_observations(forces=lapply(b,as.data.frame), indices_of_interest=1:10, last_n_milliseconds=100, force_column_names=dots_to_underscores(force_column_names))
 dcrb(means)
 cbind(time = ft_start_times, dcrb(means))

}
