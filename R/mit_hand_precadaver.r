##' includes munging of jr3
generators_from_noise_response <- function(noiseResponse_filename,jr3_null_indices,JR3_to_fingertip_distance=0.02, last_n_milliseconds=100, muscles_of_interest=muscle_names(), force_names_to_predict=dots_to_underscores(force_column_names), range_tension=c(0,20)){
    brian_noiseresponse_mit_500_maps_rep_1 <- fread(get_Resilio_filepath(noiseResponse_filename), data.table=FALSE)
     #estimated by eye Dec 1, 2017, BAC
    time_bounds <- list(tic=brian_noiseresponse_mit_500_maps_rep_1$time[head(jr3_null_indices,1)],toc=brian_noiseresponse_mit_500_maps_rep_1$time[tail(jr3_null_indices,1)])
    show_beginning_indices_up_to <- 5000 #indices corresponding to the first few seconds to show
    p <- ggplot(head(brian_noiseresponse_mit_500_maps_rep_1, show_beginning_indices_up_to)) + geom_line(aes(time, JR3_FX), col="red") + geom_line(aes(time, JR3_FY),col="green") + geom_line(aes(time, JR3_FZ), col="blue") + ggtitle("#plot to find the best time to zero jr3 signals output")
    p
    ##TODO add shaded part to show the part used for zeroing out sensors
    p + geom_rect(data=data.frame(xmin=time_bounds$tic,
                              xmax=time_bounds$toc,
                              ymin=-Inf,
                              ymax=Inf),
              aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              fill="grey",alpha=0.5)
              noise_response_wo_null <- munge_JR3_data(brian_noiseresponse_mit_500_maps_rep_1, JR3_to_fingertip_distance = JR3_to_fingertip_distance, input_are_voltages=TRUE, indices_for_null=jr3_null_indices, remove_nonzero_map_creation_ids = TRUE)

              p <- plot_measured_command_reference_over_time(noise_response_wo_null)
              ggsave(to_output_folder(paste0("xray_for_", noiseResponse_filename,".pdf")), p, width=90, height=30, limitsize=FALSE)
              noise_hand_responses <- split_by_map_and_remove_wrongly_lengthed_samples(noise_response_wo_null)
              input_output_data <- df_of_hand_response_input_output(noise_hand_responses, last_n_milliseconds)
              A_fit <- A_fit_from_80_20_split(input_output_data, muscles_of_interest, force_names_to_predict)
              generator_columns_A_matrix <- t(t(A_fit$AMatrix) %*% diag(length(muscles_of_interest)))
              compute_ranks_of_A(A_fit$AMatrix)
              binary_combinations <- custom_binary_combinations(length(muscles_of_interest),range_tension)
              binary_combination_ffs_points <- binary_combinations %*% generator_columns_A_matrix

              aspect3d(1,1,1); par3d(windowRect=c(0,0,10000,10000))
              plot_ffs_with_vertices(binary_combination_ffs_points[,1:3], generator_columns_A_matrix[,1:3], alpha_transparency=0.25, range_tension=range_tension)
              points3d(input_output_data[,force_names_to_predict][,1:3], size=1, col="black", alpha=1)
              title3d(main="Feasible Torque Set", col="black")
              rgl.snapshot(to_output_folder(paste0("plot_ffs_forces_with_vertices_", noiseResponse_filename, ".png")))
              spin_around_rgl_plot()

              rgl.clear()
              aspect3d(1,1,1); par3d(windowRect=c(0,0,10000,10000))
              plot_ffs_with_vertices(binary_combination_ffs_points[,4:6], generator_columns_A_matrix[,4:6], alpha_transparency=0.25, range_tension=range_tension)
              points3d(input_output_data[,force_names_to_predict][,4:6], size=1, col="black", alpha=1)
              title3d(main="Feasible Force Set", col="black")
              rgl.snapshot(to_output_folder(paste0("plot_ffs_torques_with_vertices_", noiseResponse_filename, ".png")))
              spin_around_rgl_plot()

              noise_response_data_fit <- list(
                noiseResponse_filename =  noiseResponse_filename,
                binary_combinations = binary_combinations,
                noise_hand_responses = noise_hand_responses,
                binary_combination_ffs_points =  binary_combination_ffs_points,
                generator_columns_A_matrix =   generator_columns_A_matrix,
                range_tension =   range_tension,
                A_fit = A_fit,
                input_output_data =   input_output_data,
                force_names_to_predict =   force_names_to_predict
              )


              return(noise_response_data_fit)
}

##' helper_plot_for_finding_generator_timepoints
##' userful for getting the right indices and showing a vertical line at each one.
##' @param timeseries_df has time column and JR3_FX column at least.
##' @param generator_indices list of numeric values representing the time point that vertical lines will be drawn. put 1 to start with something at the first second.
##' @return p ggplot line plot of JR3 signal over the full course of nrow(timeseries_df)
helper_plot_for_finding_generator_timepoints <- function(timeseries_df, generator_indices){
  ggplot(timeseries_df) + geom_line(aes(time, JR3_FX), col="red")  + geom_vline(xintercept=generator_indices) +
    scale_x_continuous(breaks = round(seq(min(timeseries_df$time), max(timeseries_df$time), by = 2),1)) + ggtitle("#plot to find the time slices where we should grab the JR3 wrench for each muscle")
}

##' indices of feasible samples
##' @inheritParams is_task_feasible AMatrix,max_allowable_residual_from_expected sset_says_task_is_infeasible
##' @return sset_list list of muscle activation pattern samples. See sset_says_task_is_infeasible
indices_of_feasible_samples <- function(sset_list, AMatrix, max_allowable_residual_from_expected){
  which(dcc(lapply(sset_list, is_task_feasible, AMatrix=AMatrix, max_allowable_residual_from_expected=max_allowable_residual_from_expected)))
}
##' based on the samples, A matrix, and the tol
##' @param samples dataframe with 7 columns (muscles) has an attr(i, "task")== structure(c(-0.0176775988084152, -0.0092413107394463, -0.103785931872721
##' '), .Names = c("JR3_FX", "JR3_FY", "JR3_FZ"))'
##' @param AMatrix matrix with columns as JR3 forces, rows as muscles.
##' @param range_tension 2 element tuple for min max across all muscles.
##' @param max_allowable_residual_from_expected numeric limit for the maximum absolute residual tolerable.
is_task_feasible <- function(samples, AMatrix, max_allowable_residual_from_expected){
  if(!(all(samples<=range_tension[2]) && all(samples >= range_tension[1]))){
    return(FALSE)
  } else {
    if(samples_create_expected_task(samples,AMatrix, max_allowable_residual_from_expected)) {
      return(TRUE)
    } else {
    stop('Muscles are within range_tension but are not accurately producing task of interest. You need to check the hit and run constraint composition.')
    }
  }
}



##' Multiple_tasks_to_sset
##' @param AMatrix result from find_A_matrix
##' @param task_df dataframe where each row is a task, and the columns are the dimensions of force.
##' @param thin mixing time for hitandrun
##' @param torque_max_deviation we desire 0 torques, so this is the max you can go from that and still get a feasible solution.
##' @param num_samples_desired number of hit and run feasible solutions to produce for each task.
##' @return sset list of samples, where each sample is a df of muscle activation patterns (each col is a muscle), and each entry has an attr(e, 'task') == e.g. num [1:3] -1.347 0.291 -5.346
multiple_tasks_to_sset <- function(AMatrix,task_df, thin=100, torque_max_deviation,num_samples_desired=1000){
  sset <- lapply(df_to_list_of_rows(task_df), function(task_i) {
    samples <- constraints_inc_torque_to_points(muscle_column_generators = t(AMatrix), range_tension=range_tension,
    task_i, num_samples_desired=num_samples_desired, thin = thin,torque_max_deviation=torque_max_deviation)
    attr(samples,'task') <- task_i
    return(samples)
  })
  return(sset)
}

##' histogram_muscle_projections to PDF based on input sset.
##' saves to "histogram_by_muscle_projections_over_5_tasks.pdf" in output folder.
##' @param sset,range_tension make sure each element of sset has the attribute "task"
histogram_muscle_projections <- function(sset, range_tension) {
pdf(to_output_folder("histogram_by_muscle_projections_over_5_tasks.pdf"), width = 100, height = 100)
  par(mfrow = c(length(sset), num_muscles))
  lapply(sset, function(samples){
    fas_histogram(samples, range_tension, attr(samples, "task"), breaks = 50, col = "black", cex = 0.25)
  })
dev.off()
}

##' Create plot to guarantee the many output forces from many muscle activation
##' patterns land exactly on the desired task. Plots via rgl plot3d.
##' @param sset set of feasible muscle activation points.
expect_five_points_in_row <- function(sset, AMatrix){
  rgl.init()
  list_of_predicted_forces <- lapply(sset, function(samples) {
    t(t(AMatrix) %*% t(samples))
  })
  plot3d(dcrb(list_of_predicted_forces))
}
##' samples_create_expected_task
##' Do all of the samples create the task of interest?
##' @param samples,AMatrix,max_allowable_residual_from_expected
##' @return logical true or false.
samples_create_expected_task <- function(samples,AMatrix, max_allowable_residual_from_expected){
  task <- attr(samples, 'task')
  all(dcc(lapply(df_to_list_of_rows(samples), function(map){
    predicted_force <- t(AMatrix) %*% as.matrix(map)
    residual <- predicted_force[1:3] - task[1:3]
    return(all(residual < max_allowable_residual_from_expected))
  })))
}



##' Filter Infeasible Tasks
##' This is useful when you find that the hit and run algorithm doesn't always
##' have a solution that will meet the torque and force criteria. but it will
##' silently pass through some muscle activation patterns that are way over
##' the range_tension.
##' @param sset,AMatrix,max_allowable_residual_from_expected see multiple_tasks_to_sset for example input classes
##' @param task_bounds = range_tension
##' @return sset_feasible same list but only with the feasible ones remaining.
filter_infeasible_tasks <- function(sset, AMatrix, max_allowable_residual_from_expected=1e-3,task_bounds){
  which_tasks_are_feasible <- indices_of_feasible_samples(sset, AMatrix, max_allowable_residual_from_expected)
  sset_feasible <- sset[which_tasks_are_feasible]
  feasible_proportion_message(sset, sset_feasible, task_bounds)
  return(sset_feasible)
}
