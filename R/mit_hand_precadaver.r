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
