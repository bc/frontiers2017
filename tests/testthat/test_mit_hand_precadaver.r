context('test_mit_hand_precadaver.r')

range_tension <- c(0,20)
force_names_to_predict <- dots_to_underscores(force_column_names)
generators_force_columns_500g <- as.matrix(read_rds_from_package_extdata("generator_force_vectors_for_each_muscle_500g_mit_hand.rds"))
binary_combinations <- custom_binary_combinations(ncol(generators_force_columns_500g), range_tension)
matrix_version_of_generators <- matrix(sapply(generators_force_columns_500g, as.numeric), ncol=7)
binary_combination_ffs_points <- matrix_version_of_generators %*% t(binary_combinations)
ffs_binary <- t(binary_combination_ffs_points)

test_that("500g recorded forces give correct FFS for forces", {
  rgl.init()
  plot_ffs_with_vertices(ffs_binary[,1:3], t(matrix_version_of_generators)[,1:3], alpha_transparency=0.25, range_tension=range_tension)
  # input_output_data <- get from random noise experiments
  # points3d(input_output_data[,force_names_to_predict][,1:3], size=1, col="black", alpha=1)
  title3d(main="FFS", xlab="Fx", ylab="Fy", zlab="Fz", col="black")
})

test_that("500g recorded forces give correct FFS for forces", {
  rgl.init()
  plot_ffs_with_vertices(ffs_binary[,4:6], t(matrix_version_of_generators)[,4:6], alpha_transparency=0.25, range_tension=range_tension)
  # input_output_data <- get from random noise experiments
  # points3d(input_output_data[,force_names_to_predict][,1:3], size=1, col="black", alpha=1)
  title3d(main="FFS for Torques", xlab="Mx", ylab="My", zlab="Mz", col="black")
})


test_that("500g second trial done on Nov30 works for FFS for forces", {
  brian_timeseries_with_500g_3reps <- fread(get_Resilio_filepath("noiseResponse2017_11_30_19_07_22_500g_mit_hand.txt"), data.table=FALSE)
  ggplot(brian_timeseries_with_500g_3reps) + geom_line(aes(time, JR3_FX), col="red") + geom_line(aes(time, JR3_FY),col="green") + geom_line(aes(time, JR3_FZ), col="blue") + ggtitle("#plot to find the best time to zero jr3 signals output")
  zeroed_500g_df <- munge_JR3_data(brian_timeseries_with_500g_3reps, input_are_voltages = TRUE, indices_for_null = 1:4000, remove_nonzero_map_creation_ids = FALSE)
  generator_indices <- c(19,31,42,53,65,76,88)
  helper_plot_for_finding_generator_timepoints(zeroed_500g_df,generator_indices)
  ggplot(zeroed_500g_df) + geom_line(aes(time, JR3_FX), col="red") + geom_line(aes(time, JR3_FY),col="green") + geom_line(aes(time, JR3_FZ), col="blue") + geom_line(aes(time, JR3_MX),col="orange") + geom_line(aes(time, JR3_MY),col="gray") + geom_line(aes(time, JR3_MZ),col="purple") + geom_vline(xintercept=generator_indices) + xlab("Time") + ylab('Force in Newtons, Newton-Meters') + ggtitle('show forces at each trial, with slice for each wrench')
    snapshots <- take_running_mean_snapshots(zeroed_500g_df,zeroed_500g_df$time, generator_indices, n_samples_for_running_mean=30)
    colnames(snapshots) <- measured(muscle_names())
    matrix_version_of_generators <- matrix(sapply(snapshots, as.numeric), ncol=7)
    forces_from_the_vertices_of_feasible_activation_space <- t(matrix_version_of_generators %*% t(binary_combinations))
    colnames(forces_from_the_vertices_of_feasible_activation_space) <- dots_to_underscores(force_column_names)


    animation_time = 120
    aspect3d(1/5,1/5,1/5); par3d(windowRect=c(0,0,10000,10000))
    plot_ffs_with_vertices(forces_from_the_vertices_of_feasible_activation_space[,1:3], t(matrix_version_of_generators)[,1:3], alpha_transparency=0.25, range_tension=c(0,20))
    rgl.snapshot(to_output_folder("plot_ffs_forces_with_vertices_noiseResponse2017_11_30_19_07_22_500g_mit_hand.png"))
    spin_around_rgl_plot(animation_time)
    rgl.clear()
    aspect3d(1,1,1); par3d(windowRect=c(0,0,10000,10000))
    plot_ffs_with_vertices(forces_from_the_vertices_of_feasible_activation_space[,4:6], t(matrix_version_of_generators)[,4:6], alpha_transparency=0.25, range_tension=c(0,20))
    rgl.snapshot(to_output_folder("plot_ffs_torques_with_vertices_noiseResponse2017_11_30_19_07_22_500g_mit_hand.png"))
    spin_around_rgl_plot(animation_time)

})

test_that("noiseResponse vectors can be plotted in 3D for 500 maps, 1 replicate per map", {
  noise_response_model_and_data <- generators_from_noise_response("noiseResponse2017_11_30_20_16_06_500_maps_reps_1.txt", jr3_null_indices = c(500:750))
})

test_that("compare noiseResponse2017_11_30_20_16_06_500_maps_reps_1.txt with 500g manual generators", {
  noise_response_model_and_data <- generators_from_noise_response("noiseResponse2017_11_30_20_16_06_500_maps_reps_1.txt", jr3_null_indices = c(500:750))

  animation_time = 120
  brian_timeseries_with_500g_3reps <- fread(get_Resilio_filepath("noiseResponse2017_11_30_19_07_22_500g_mit_hand.txt"), data.table=FALSE)
  ggplot(brian_timeseries_with_500g_3reps) + geom_line(aes(time, JR3_FX), col="red") + geom_line(aes(time, JR3_FY),col="green") + geom_line(aes(time, JR3_FZ), col="blue") + ggtitle("#plot to find the best time to zero jr3 signals output")
  zeroed_500g_df <- munge_JR3_data(brian_timeseries_with_500g_3reps, input_are_voltages = TRUE, indices_for_null = 1:4000, remove_nonzero_map_creation_ids = FALSE)
  generator_indices <- c(19,31,42,53,65,76,88)
  helper_plot_for_finding_generator_timepoints(zeroed_500g_df,generator_indices)
  ggplot(zeroed_500g_df) + geom_line(aes(time, JR3_FX), col="red") + geom_line(aes(time, JR3_FY),col="green") + geom_line(aes(time, JR3_FZ), col="blue") + geom_line(aes(time, JR3_MX),col="orange") + geom_line(aes(time, JR3_MY),col="gray") + geom_line(aes(time, JR3_MZ),col="purple") + geom_vline(xintercept=generator_indices) + xlab("Time") + ylab('Force in Newtons, Newton-Meters') + ggtitle('show forces at each trial, with slice for each wrench')
    snapshots <- take_running_mean_snapshots(zeroed_500g_df,zeroed_500g_df$time, generator_indices, n_samples_for_running_mean=30)
    colnames(snapshots) <- measured(muscle_names())
    matrix_version_of_generators <- matrix(sapply(snapshots, as.numeric), ncol=7)
    scaled_matrix_version_of_generators <- matrix_version_of_generators*0.05
    forces_from_the_vertices_of_feasible_activation_space <- t(scaled_matrix_version_of_generators %*% t(binary_combinations))
    colnames(forces_from_the_vertices_of_feasible_activation_space) <- dots_to_underscores(force_column_names)


    #FFS Forces from 500g
    rgl.clear(); aspect3d(1/5,1/5,1/5); par3d(windowRect=c(0,0,10000,10000))
    plot_ffs_with_vertices(forces_from_the_vertices_of_feasible_activation_space[,1:3], t(scaled_matrix_version_of_generators)[,1:3], alpha_transparency=0.25, range_tension=c(0,20))
    rgl.snapshot(to_output_folder("plot_ffs_forces_with_vertices_noiseResponse2017_11_30_19_07_22_500g_mit_hand.png"))
    # spin_around_rgl_plot(animation_time)
    #FFS forces from noiseResponse
    observed_forces <- noise_response_model_and_data$input_output_data[,force_names_to_predict]
    points3d(observed_forces[,1:3], col="blue", size=7)

    #FTS torques from 500g
    rgl.clear(); aspect3d(1/5,1/5,1/5); par3d(windowRect=c(0,0,10000,10000))
    plot_ffs_with_vertices(forces_from_the_vertices_of_feasible_activation_space[,4:6], t(scaled_matrix_version_of_generators)[,4:6], alpha_transparency=0.25, range_tension=c(0,20))
    rgl.snapshot(to_output_folder("plot_ffs_forces_with_vertices_noiseResponse2017_11_30_19_07_22_500g_mit_hand.png"))
    # spin_around_rgl_plot(animation_time)
    #FTS torques from noiseResponse
    observed_forces <- noise_response_model_and_data$input_output_data[,force_names_to_predict]
    points3d(observed_forces[,4:6], col="black", size=7)



})

test_that("when tendons are fixed to posts the signals look acceptable",{
    timeseries_tendons_to_posts <- fread(get_Resilio_filepath("noiseResponse2017_11_30_19_59_30_tendons_to_posts_mit8pm.txt"), data.table=FALSE)
      p1 <- ggplot(tail(timeseries_tendons_to_posts,100)) + geom_line(aes(time,JR3_FX)) + geom_line(aes(time,JR3_FY)) + geom_line(aes(time,JR3_FZ)) + geom_line(aes(time,JR3_MX)) + geom_line(aes(time,JR3_MY)) + geom_line(aes(time,JR3_MZ))
      p2 <- ggplot(tail(timeseries_tendons_to_posts,4000)) + geom_line(aes(time,measured_M0)) + geom_line(aes(time,measured_M1)) + geom_line(aes(time,measured_M2)) + geom_line(aes(time,measured_M3)) + geom_line(aes(time,measured_M4)) + geom_line(aes(time,measured_M5))+ geom_line(aes(time,measured_M6))
      ggsave(to_output_folder("tendons_fixed_to_posts.pdf"), arrangeGrob(p1,p2))
      #TODO stability metrics for last 100ms of each map
})


test_that("when we get noise through the MIT hand (500 forces, 1 replicate), I can make an A matrix", {
last_n_milliseconds<-100
muscles_of_interest <- muscle_names()
force_names_to_predict <- dots_to_underscores(force_column_names)
untransformed_noise_response <- fread(get_Resilio_filepath("noiseResponse2017_11_30_20_16_06_500_maps_reps_1.txt"), data.table=FALSE)
noise_response_wo_null <- munge_JR3_data(untransformed_noise_response, input_are_voltages=TRUE, indices_for_null=50:250)
p <- plot_measured_command_reference_over_time(noise_response_wo_null)
ggsave(to_output_folder("xray_for_noiseReponse_nov30_mit_hand.pdf"), p, width=90, height=30, limitsize=FALSE)
noise_hand_responses <- split_by_map_and_remove_wrongly_lengthed_samples(noise_response_wo_null)
input_output_data <- df_of_hand_response_input_output(noise_hand_responses, last_n_milliseconds)
A_fit <- A_fit_from_80_20_split(input_output_data, muscles_of_interest, force_names_to_predict)
p1 <- ggplot(tail(timeseries_tendons_to_posts,100)) + geom_line(aes(time,JR3_FX)) + geom_line(aes(time,JR3_FY)) + geom_line(aes(time,JR3_FZ)) + geom_line(aes(time,JR3_MX)) + geom_line(aes(time,JR3_MY)) + geom_line(aes(time,JR3_MZ))
p2 <- ggplot(tail(timeseries_tendons_to_posts,4000)) + geom_line(aes(time,measured_M0)) + geom_line(aes(time,measured_M1)) + geom_line(aes(time,measured_M2)) + geom_line(aes(time,measured_M3)) + geom_line(aes(time,measured_M4)) + geom_line(aes(time,measured_M5))+ geom_line(aes(time,measured_M6))
ggsave(to_output_folder("tendons_fixed_to_posts.pdf"), arrangeGrob(p1,p2))
message("noiseResponse A Matrix")
print(t(A_fit$AMatrix))

})
test_that("when we run 5 forces 100 times each, I can compute the by-force-dimension variabilty of the output wrench vector", {

})


test_that("when we get binary combinations through the MIT hand, I can make an A matrix from the single value rows where only one muscle is pulled on", {
"single_muscle_binary_combinations_5_replicates_5_levels_35_total_forces.csv"
})
test_that("when we get binary combinations through the MIT hand, I can make an A matrix from all binary combinations", {

})


test_that('we can produce binary combinations csv to run alonside noise', {
 combinations_binary <- custom_binary_combinations(7,c(0,20))
  binary_table <- create_and_cbind_map_creation_ids(combinations_binary, muscle_names())
  write.csv(binary_table, to_output_folder("binary_combinations_no_replicates.csv"),
    row.names = FALSE, quote = FALSE)
})
