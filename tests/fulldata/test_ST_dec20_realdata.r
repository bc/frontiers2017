context('real data collected dec 20')
context("test_spatiotemporal_AFit_NN_LRM.r")

# noiseResponse_ST1BC_2017_12_20_13_09_42.txt is MIT hand in flexish posture

set.seed(4)
response_from_first_hand <- fread(get_Resilio_filepath("noiseResponse_ST1BC_2017_12_20_18_19_39_ST_500_parallel_maps_good_3tapjr3null.txt"), data.table=FALSE)

JR3_to_fingertip_distance <- 0.00802 #about 9mm in meters TODO

filename_2A <- "noiseResponse_ST1BC_2017_12_20_18_19_39_ST_500_parallel_maps_good_3tapjr3null"
  last_n_milliseconds <- 100
  range_tension <- c(0, 10)
  muscles_of_interest <- muscle_names()
  num_muscles <- length(muscles_of_interest)
  force_names_to_predict <- c("JR3_FX","JR3_FY","JR3_FZ","JR3_MX","JR3_MY","JR3_MZ")
  untransformed_noise_response <- response_from_first_hand
  indices_for_null <- 429412:439410 #TODO
  # To get indices for null from timepoints
  # which(untransformed_noise_response$time == 430)
  # which(untransformed_noise_response$time == 440)

  untransformed_p <- plot_measured_command_reference_over_time(untransformed_noise_response[untransformed_noise_response$time > 430 & untransformed_noise_response$time < 440,])
  ggsave(to_output_folder(paste0("get_null_indices_via_this_plot_of_untransformed_xray_for_",filename_2A ,".pdf")), untransformed_p, width=90, height=30, limitsize=FALSE)
  # colMeans(untransformed_noise_response[indices_for_null,]) - JR3_null_from_1A # shows few differences in JR3 sensor vals over time
  noise_response_wo_null <- munge_JR3_data(untransformed_noise_response, input_are_voltages=TRUE, JR3_to_fingertip_distance=JR3_to_fingertip_distance, indices_for_null=indices_for_null)
  p <- plot_measured_command_reference_over_time(noise_response_wo_null)
  ggsave(to_output_folder(paste0("xray_for_",filename_2A ,".pdf")), p, width=90, height=30, limitsize=FALSE)
  noise_hand_responses <- split_by_map_and_remove_wrongly_lengthed_samples(noise_response_wo_null)
  input_output_data <- df_of_hand_response_input_output(noise_hand_responses, last_n_milliseconds)

  write.csv(input_output_data,to_output_folder(paste0("input_output_data_for_",filename_2A, ".csv")), row.names=FALSE)



  A_fit <- A_fit_from_80_20_split(input_output_data, muscles_of_interest, force_names_to_predict)
  generator_columns_A_matrix <- t(t(A_fit$AMatrix) %*% diag(num_muscles))
  compute_ranks_of_A(A_fit$AMatrix)
  # Here, identify a force vector of interest and apply it to the generated A
  binary_combinations <- custom_binary_combinations(num_muscles,range_tension)
  binary_combination_ffs_points <- binary_combinations %*% generator_columns_A_matrix
  aspect3d(1/5,1/5,1/5); par3d(windowRect=c(0,0,10000,10000))
  plot_ffs_with_vertices(binary_combination_ffs_points[,1:3], generator_columns_A_matrix[,1:3], alpha_transparency=0.25, range_tension=range_tension)
  points3d(input_output_data[,force_names_to_predict][,1:3], size=1, col="black", alpha=1)
  title3d(main="FFS", xlab="Fx", ylab="Fy", zlab="Fz", col="black")


  horizontal_line_points <- identify_n_points_from_pointcloud(binary_combination_ffs_points[,1:3],n=2)
  horizontal_line_tasks <- dcrb(draw_perpendicular_line(horizontal_line_points[1,],horizontal_line_points[2,],10))
  spheres3d(horizontal_line_tasks, r=0.10, col="pink")
  message('Pick 1 point to define the scaling direction.')
  task_direction_to_scale <- identify_n_points_from_pointcloud(binary_combination_ffs_points[,1:3],n=1)
  map_that_created_task_dir <- binary_combinations[which(binary_combination_ffs_points[,1]==task_direction_to_scale[1]),]
  line_tasks_for_2A <- as.matrix(t(t(A_fit$AMatrix) %*% t(binary_combinations[c(13,20),])))[,1:3]
  task_information_collected_by_brian <- list(horizontal_line_points=horizontal_line_points, horizontal_line_tasks=horizontal_line_tasks,task_direction_to_scale=task_direction_to_scale,map_that_created_task_dir=map_that_created_task_dir,line_tasks_for_2A=line_tasks_for_2A)
  # Only run on experiment day to save the final val. then comment out the above:
  # saveRDS(task_information_collected_by_brian, paste0("tasks_from_flexed_position_via",filename_2A,".rds")
  #Procedure: Send input_output_CSV, tasks



    horizontal_line_points <-  task_information_collected_by_brian$horizontal_line_points
    horizontal_line_tasks <-  task_information_collected_by_brian$horizontal_line_tasks
    task_direction_to_scale <-  task_information_collected_by_brian$task_direction_to_scale
    map_that_created_task_dir <-  task_information_collected_by_brian$map_that_created_task_dir

    #Now show the torque FAS just to show what dimensions it is constrained in.
    rgl.init()
    plot_ffs_with_vertices(binary_combination_ffs_points[,4:6], generator_columns_A_matrix[,4:6], alpha_transparency=0.25, range_tension=range_tension)

    ############ MANUAL: IDENTIFY TASK MULTIPLIER BOUNDS FOR SCALING
    task_bounds <- c(1e-2, 1)
    num_samples_desired <- 10
    num_tasks <- 9
    task_multiplier_list <- seq(task_bounds[1], task_bounds[2], length.out = num_tasks)
    task_df <- t(task_direction_to_scale %*% t(task_multiplier_list))
    colnames(task_df) <- force_names_to_predict[1:3]
    sset_scaling <- multiple_tasks_to_sset(A_fit$AMatrix,task_df, thin=100, torque_max_deviation=0.05, num_samples_desired=num_samples_desired)
    sset_feasible_scaling <- filter_infeasible_tasks(sset_scaling, A_fit$AMatrix, max_allowable_residual_from_expected=1e-3, task_bounds=task_bounds)
    task_list_from_sset_list(sset_feasible_scaling)



    l1_cost_limits <- lapply(sset_feasible_scaling, l1_cost_limits)
    how_muscle_lower_bound_changes <- dcc(lapply(l1_cost_limits, function(lo_hi){
      lo_hi[1,2]
    }))

    #select which indices from the FAS you should use to make sure output len is 5
    sset_feasible_scaling <- sset_feasible_scaling[c(1,2,3,4,5)]
    five_scaling_tasks_to_give_to_ali <- task_list_from_sset_list(sset_feasible_scaling)
    colnames(five_scaling_tasks_to_give_to_ali) <- c("JR3_FX", "JR3_FY", "JR3_FZ")


    ############ MANUAL: IDENTIFY TASK MULTIPLIER BOUNDS FOR HORIZONTAL
      num_samples_desired <- 10
      scale_factor_for_magnitude_of_all_horizontal_forces <- 1.0
      colnames(horizontal_line_tasks) <- force_names_to_predict[1:3]
      scaled_horizontal_line_tasks <- scale_factor_for_magnitude_of_all_horizontal_forces* horizontal_line_tasks
      sset_scaling_horizontal <- multiple_tasks_to_sset(A_fit$AMatrix,scaled_horizontal_line_tasks, thin=100, torque_max_deviation=0.1, num_samples_desired=num_samples_desired)
      sset_feasible_horizontal <- filter_infeasible_tasks(sset_scaling_horizontal, A_fit$AMatrix, max_allowable_residual_from_expected=1e-3, task_bounds = c(scale_factor_for_magnitude_of_all_horizontal_forces, scale_factor_for_magnitude_of_all_horizontal_forces))

    #wait, which ones were feasible?
      five_horizontal_tasks_to_give_to_ali <- dcrb(lapply(sset_feasible_horizontal, function(samples){
        return(attr(samples,'task'))
      }))
      colnames(five_horizontal_tasks_to_give_to_ali) <- c("JR3_FX","JR3_FY","JR3_FZ")
      indices_of_horizontal_to_select <- c(1,2,3,4,5) #TAKE five of them, make sure they're evenly spaced.
      five_horizontal_tasks_to_give_to_ali <-five_horizontal_tasks_to_give_to_ali[indices_of_horizontal_to_select,]


############################
#PREP & DELIVER TASK LIST FOR ALI. 5 for scaling, then 5 rows for horizontal task. map ids that start with 1 are from LRM, map id's that start with 2 are from the NN.
      write.csv(rbind(five_scaling_tasks_to_give_to_ali,five_horizontal_tasks_to_give_to_ali), to_output_folder(paste0("five_scaling_and_five_horizontal_tasks_from", filename_2A,".csv")), row.names=FALSE)
############################

    # Wait for ali to produce his tasks..........................................................................
    # .......................# .......................# .......................# .......................
    # .......................# .......................# .......................# .......................
    # .......................# .......................# .......................# .......................


      ##########PREP MAPS FOR SCALING ###############
      res_scaling <- lapply(sset_feasible_scaling, create_and_cbind_map_creation_ids, muscles_of_interest)
      big_har_set_to_test_on_finger_scaling <- dcrb(res_scaling)

      ##########PREP MAPS FOR HORIZONTAL REDIRECTION TASK ###############
      res_horizontal <- lapply(sset_feasible_horizontal[indices_of_horizontal_to_select], create_and_cbind_map_creation_ids, muscles_of_interest)
      big_har_set_to_test_on_finger_horizontal <- dcrb(res_horizontal)

    #bring them all together
      jumbo_task_map_validations_set_name <- "all_task_validations_concatenated_Ascaling50_Ahorizontal50_LRM_scaling5_LRM_horizontal5_NN_scaling_5_NN_scaling_5_total_CHECKNUMFORCES.csv"

      jumbo_concatenated_set <- rbind(big_har_set_to_test_on_finger_horizontal,
                                      big_har_set_to_test_on_finger_scaling)
      write.csv(jumbo_concatenated_set, to_output_folder(jumbo_task_map_validations_set_name),row.names = FALSE, quote = FALSE)
      #IMPORTANT: then manually add the rows from Ali, and use that as your Num Forces

      #now that the length has changed, use nrow to get the NUM_FORCES
      nrow(read.csv('output/all_task_validations_concatenated_Ascaling50_Ahorizontal50_LRM_scaling5_LRM_horizontal5_NN_scaling_5_NN_scaling_5_total_CHECKNUMFORCES_withaliforces.csv'))
