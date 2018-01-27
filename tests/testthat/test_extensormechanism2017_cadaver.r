context('test_extensormechanism2017_cadaver.r')


context('extract null from solid file to apply to all trials')
  extensormechanism_1c_filename_jr3_null_test <- "noiseResponse2017_12_04_03_57_54_EXMECH_1C_oncadaver_3tap.txt"
  extmech_1c_path <- paste0('dec4_extensor_mechanism_input_and_response/1/', extensormechanism_1c_filename_jr3_null_test)
  untransformed_noise_response <- fread(get_Resilio_filepath(extmech_1c_path), data.table=FALSE)
  indices_for_null <- 1323640:1326639
  untransformed_p <- plot_measured_command_reference_over_time(untransformed_noise_response[indices_for_null,])
  ggsave(to_output_folder(paste0("get_null_indices_of_beginning_via_this_plot_of_untransformed_xray_for_",extensormechanism_1c_filename_jr3_null_test ,".pdf")), untransformed_p, width=90, height=30, limitsize=FALSE)

############Useful global variables
  JR3_null_from_1A <- colMeans(untransformed_noise_response[indices_for_null,])
  tasks_from_flexed_position_via_1A <- read_rds_from_package_extdata("tasks_from_flexed_position_via_1A.rds")
############Useful global variables



context("Stage 0: run tendons against post")
test_that("0A looks good",{
  filename_0A <- "noiseResponse2017_12_04_02_14_43_EXMECH_0A_nmaps500_on_posts.txt"
})
test_that("0B looks good",{
  filename_0B <- "noiseResponse2017_12_04_02_28_17_EXMECH_0B_onposts.txt"
})


######################################################################################
######################################################################################
######################################################################################
context("Stage 1: Flexed posture: run tendons against hand, only extensor muscles")
test_that("1A looks good, create new activations for 1A and decide on task for 2A",{
  last_n_milliseconds <- 100
  range_tension <- c(0, 10)
  muscles_of_interest <- muscle_names()[1:5]
  num_muscles <- length(muscles_of_interest)
  force_names_to_predict <- c("JR3_FX","JR3_FY","JR3_FZ","JR3_MX","JR3_MY","JR3_MZ")
  noise_response_filename <- "noiseResponse2017_12_04_02_53_10_EXMECH_1A_oncadaver_3tap.txt"
  filepath_noiseresponse <- paste0('dec4_extensor_mechanism_input_and_response/1/', noise_response_filename)

  untransformed_noise_response <- as.data.frame(fread(get_Resilio_filepath(filepath_noiseresponse)))
  untransformed_p <- plot_measured_command_reference_over_time(untransformed_noise_response)
  ggsave(to_output_folder(paste0("get_null_indices_via_this_plot_of_untransformed_xray_for_",noise_response_filename ,".pdf")), untransformed_p, width=90, height=30, limitsize=FALSE)

  noise_response_wo_null <- munge_JR3_data(untransformed_noise_response, input_are_voltages=TRUE, JR3_to_fingertip_distance=0.00956, indices_for_null=3000:10000,JR3_sensor_null=JR3_null_from_1A)
  p <- plot_measured_command_reference_over_time(noise_response_wo_null)
  ggsave(to_output_folder(paste0("xray_for_",noise_response_filename ,".pdf")), p, width=90, height=30, limitsize=FALSE)
  noise_hand_responses <- split_by_map_and_remove_wrongly_lengthed_samples(noise_response_wo_null)
  input_output_data <- df_of_hand_response_input_output(noise_hand_responses, last_n_milliseconds)
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

  browser()
  message('Pick 2 points to define the horizontal line endpoints.')

#this part was done manually, resulting in "tasks_from_flexed_position_via_1A.rds". BC 05h19
  # horizontal_line_points <- identify_n_points_from_pointcloud(binary_combination_ffs_points[,1:3],n=2)
  # horizontal_line_tasks <- dcrb(draw_perpendicular_line(horizontal_line_points[1,],horizontal_line_points[2,],10))
  # spheres3d(horizontal_line_tasks, r=0.10, col="pink")
  # message('Pick 1 point to define the scaling direction.')
  # task_direction_to_scale <- identify_n_points_from_pointcloud(binary_combination_ffs_points[,1:3],n=1)
  # map_that_created_task_dir <- binary_combinations[which(binary_combination_ffs_points[,1]==task_direction_to_scale[1]),]
  # line_tasks_for_2A <- as.matrix(t(t(A_fit$AMatrix) %*% t(binary_combinations[c(13,20),])))[,1:3]
  # tasks_from_flexed_position <- list(horizontal_line_points=horizontal_line_points, horizontal_line_tasks=horizontal_line_tasks,task_direction_to_scale=task_direction_to_scale,map_that_created_task_dir=map_that_created_task_dir,line_tasks_for_2A=line_tasks_for_2A)
  # saveRDS(tasks_from_flexed_position, "tasks_from_flexed_position_via_1A.rds")

  horizontal_line_points <-  tasks_from_flexed_position_via_1A$horizontal_line_points
  horizontal_line_tasks <-  tasks_from_flexed_position_via_1A$horizontal_line_tasks
  task_direction_to_scale <-  tasks_from_flexed_position_via_1A$task_direction_to_scale
  map_that_created_task_dir <-  tasks_from_flexed_position_via_1A$map_that_created_task_dir


  index_of_closest_fit_to_line_endpoint_A <- which.min(dcc(lapply(df_to_list_of_rows(binary_combination_ffs_points[,1:3]), function(row_i){
    norm_vec(row_i - c(3.168059,  0.2386046, -0.1192935))
  })))
  index_of_closest_fit_to_line_endpoint_B <- which.min(dcc(lapply(df_to_list_of_rows(binary_combination_ffs_points[,1:3]), function(row_i){
    norm_vec(row_i - c(1.328577, -0.2079993, -1.9833277))
  })))



  #Now show the torque FAS just to show what dimensions it is constrained in.
  rgl.init()
  plot_ffs_with_vertices(binary_combination_ffs_points[,4:6], generator_columns_A_matrix[,4:6], alpha_transparency=0.25, range_tension=range_tension)

  ############ MANUAL: IDENTIFY TASK MULTIPLIER BOUNDS FOR SCALING
  task_bounds <- c(1e-2, 1)
  num_samples_desired <- 100
  num_tasks <- 10
  task_multiplier_list <- seq(task_bounds[1], task_bounds[2], length.out = num_tasks)
  task_df <- t(task_direction_to_scale %*% t(task_multiplier_list))
  colnames(task_df) <- force_names_to_predict[1:3]
  sset_scaling <- multiple_tasks_to_sset(A_fit$AMatrix,task_df, thin=100, torque_max_deviation=0.05, num_samples_desired=num_samples_desired)
  sset_feasible_scaling <- filter_infeasible_tasks(sset_scaling, A_fit$AMatrix, max_allowable_residual_from_expected=1e-3, task_bounds=task_bounds)
  dcrb(lapply(sset_feasible_scaling, function(samples){
    boxplot(samples)
    return(attr(samples,'task'))
  }))

  l1_cost_limits <- lapply(sset_feasible_scaling, l1_cost_limits)
  how_muscle_lower_bound_changes <- dcc(lapply(l1_cost_limits, function(lo_hi){
    lo_hi[1,2]
  }))

  #TAKE EVERY 3rd POINT BC CAD 18h24
  sset_feasible_scaling <- sset_feasible_scaling[c(3,4,5,6,7)]

  #sanity check plots
  plot(how_muscle_lower_bound_changes, type='l',ylab="M0 in lowest l1 soln", xlab="Task Intensity")
  histogram_muscle_projections(sset_feasible_scaling, range_tension)
  expect_five_points_in_row(sset_feasible_scaling, A_fit$AMatrix)

  ####### Visualize the FAS sets that are produced by feeding in the noiseResponse data.
  rgl.clear()
  num_tasks <- length(sset_feasible_scaling)
  rgl_init(bg = "white")
  extract_3cols <- lapply(sset_feasible_scaling, function(x) x[, c(1, 2, 3)])
  # extract_3cols[[1]] <- NULL #remove the 00000 force
  gradient <- colorRampPalette(c("#a6cee3", "#1f78b4", "#b2df8a", "#fc8d62", "#ffffb3",
    "#bebada"))
  list_of_mats <- add_gradient_to_attrs(extract_3cols, gradient(length(extract_3cols)))
  list_of_mats <- lapply(list_of_mats, function(x) x)

  axes_for_multiple_sets(list_of_mats)
  axes_for_defined_xyz_limits(rep(list(range_tension), 3))
  rgl_convhulls(list_of_mats, points = TRUE)
zero_vec_for_flexors <- rep(0, nrow(big_har_set_to_test_on_finger))
zeros_for_flexors <- data.frame(M5 = zero_vec_for_flexors, M6 = zero_vec_for_flexors)
  ##########PREP CSV MAPS FOR SCALING ###############
  res <- lapply(sset_feasible_scaling, create_and_cbind_map_creation_ids, muscles_of_interest)
  big_har_set_to_test_on_finger <- dcrb(res)
  har_set_with_zeros_for_flexors <- cbind(big_har_set_to_test_on_finger, zeros_for_flexors)
  filename_to_save_har_set <- "scaling_1E_05h30.csv"
  write.csv(har_set_with_zeros_for_flexors, to_output_folder(filename_to_save_har_set),
    row.names = FALSE, quote = FALSE)

  #sanity check:
  expect_five_points_in_row_for_csv_maps(filename = filename_to_save_har_set, A_fit=A_fit)




  ############ MANUAL: IDENTIFY TASK MULTIPLIER BOUNDS FOR HORIZONTAL
    num_samples_desired <- 100
    scale_factor_for_magnitude_of_all_horizontal_forces <- 1.0
    colnames(horizontal_line_tasks) <- force_names_to_predict[1:3]
    scaled_horizontal_line_tasks <- scale_factor_for_magnitude_of_all_horizontal_forces* horizontal_line_tasks
    sset_scaling_horizontal <- multiple_tasks_to_sset(A_fit$AMatrix,scaled_horizontal_line_tasks, thin=100, torque_max_deviation=0.1, num_samples_desired=num_samples_desired)
    sset_feasible_horizontal <- filter_infeasible_tasks(sset_scaling_horizontal, A_fit$AMatrix, max_allowable_residual_from_expected=1e-3, task_bounds = c(scale_factor_for_magnitude_of_all_horizontal_forces, scale_factor_for_magnitude_of_all_horizontal_forces))

  #wait, which ones were feasible?
    dcrb(lapply(sset_feasible_horizontal, function(samples){
      boxplot(samples)
      return(attr(samples,'task'))
    }))

    ##########PREP CSV MAPS FOR HORIZONTAL REDIRECTION TASK ###############
    res <- lapply(sset_feasible_horizontal[c(1,3,5,7,9)], create_and_cbind_map_creation_ids, muscles_of_interest)
    big_har_set_to_test_on_finger_horizontal <- dcrb(res)
    har_set_with_zeros_for_flexors <- cbind(big_har_set_to_test_on_finger_horizontal, zeros_for_flexors)
    filename_for_horizontal <- "horizontal_1F_05h36.csv"
    write.csv(har_set_with_zeros_for_flexors, to_output_folder(filename_for_horizontal),row.names = FALSE, quote = FALSE)

  #sanity check for horizontal
  expect_five_points_in_row_for_csv_maps(filename = filename_for_horizontal, A_fit=A_fit, muscles_of_interest)


})
test_that("1B looks good",{
  filename_1B <- "noiseResponse2017_12_04_03_04_10_EXMECH_1B_oncadaver_all_good.txt"
})
test_that("1C looks good",{
  filename_1C <- "noiseResponse2017_12_04_03_57_54_EXMECH_1C_oncadaver_all_good_3tap_JR3_null_at_end.txt"
})
test_that("1D looks good",{
  filename_1D <- "noiseResponse2017_12_04_04_24_41_EXMECH_1D_oncadaver_all_god_3tap_JR3_null_at_end.txt"
})
test_that("1E looks good",{
  filename_1E <- "noiseResponse2017_12_04_05_39_58_EXMECH_1E_3tap.txt"
})
test_that("1F looks good",{
  filename_1F <- "noiseResponse2017_12_04_05_52_59_EXMECH_1F_3tap.txt"
})
######################################################################################
######################################################################################
######################################################################################
######################################################################################
context("Stage:2 Flexed Posture: run tendons against hand, all muscles, using task for stage 1")
test_that("2A looks good",{
  filename_2A <- "noiseResponse2017_12_04_06_03_59_EXMECH_2A_3tap.txt"
  path2A <- paste0('dec4_extensor_mechanism_input_and_response/2/', filename_2A)
  last_n_milliseconds <- 100
  range_tension <- c(0, 10)
  muscles_of_interest <- muscle_names()
  num_muscles <- length(muscles_of_interest)
  force_names_to_predict <- c("JR3_FX","JR3_FY","JR3_FZ","JR3_MX","JR3_MY","JR3_MZ")
  untransformed_noise_response <- as.data.frame(fread(get_Resilio_filepath(path2A)))
  indices_for_null <- 425950:431084
  untransformed_p <- plot_measured_command_reference_over_time(untransformed_noise_response[untransformed_noise_response$time > 430 & untransformed_noise_response$time < 435.135,])
  ggsave(to_output_folder(paste0("get_null_indices_via_this_plot_of_untransformed_xray_for_",filename_2A ,".pdf")), untransformed_p, width=90, height=30, limitsize=FALSE)
  # colMeans(untransformed_noise_response[indices_for_null,]) - JR3_null_from_1A # shows few differences in JR3 sensor vals over time
  noise_response_wo_null <- munge_JR3_data(untransformed_noise_response, input_are_voltages=TRUE, JR3_to_fingertip_distance=0.00956, indices_for_null=indices_for_null)
  p <- plot_measured_command_reference_over_time(noise_response_wo_null)
  ggsave(to_output_folder(paste0("xray_for_",filename_2A ,".pdf")), p, width=90, height=30, limitsize=FALSE)
  noise_hand_responses <- split_by_map_and_remove_wrongly_lengthed_samples(noise_response_wo_null)
  input_output_data <- df_of_hand_response_input_output(noise_hand_responses, last_n_milliseconds)
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
##################################################
##################################################
#BRING IN TASKS FROM 1A and create scaling, horizontal.
  tasks_from_flexed_position_via_1A <- read_rds_from_package_extdata("tasks_from_flexed_position_via_1A.rds")
  task_direction_to_scale <-  tasks_from_flexed_position_via_1A$task_direction_to_scale
  map_that_created_task_dir <-  tasks_from_flexed_position_via_1A$map_that_created_task_dir

  task_direction_to_scale <- as.numeric(t(t(A_fit$AMatrix) %*% c(map_that_created_task_dir,0,0))[1:3])
  names(task_direction_to_scale) <- c("JR3_FX","JR3_FY", "JR3_FZ")

  #Now show the torque FAS just to show what dimensions it is constrained in.
  rgl.init()
  plot_ffs_with_vertices(binary_combination_ffs_points[,4:6], generator_columns_A_matrix[,4:6], alpha_transparency=0.25, range_tension=range_tension)

  ############ MANUAL: IDENTIFY TASK MULTIPLIER BOUNDS FOR SCALING
  task_bounds <- c(1e-2, 2)
  num_samples_desired <- 100
  num_tasks <- 10
  task_multiplier_list <- seq(task_bounds[1], task_bounds[2], length.out = num_tasks)
  task_df <- t(task_direction_to_scale %*% t(task_multiplier_list))
  colnames(task_df) <- force_names_to_predict[1:3]
  sset_scaling <- multiple_tasks_to_sset(A_fit$AMatrix,task_df, thin=100, torque_max_deviation=0.05, num_samples_desired=num_samples_desired)
  sset_feasible_scaling <- filter_infeasible_tasks(sset_scaling, A_fit$AMatrix, max_allowable_residual_from_expected=1e-3, task_bounds=task_bounds)
  dcrb(lapply(sset_feasible_scaling, function(samples){
    boxplot(samples)
    return(attr(samples,'task'))
  }))

  l1_cost_limits <- lapply(sset_feasible_scaling, l1_cost_limits)
  how_muscle_lower_bound_changes <- dcc(lapply(l1_cost_limits, function(lo_hi){
    lo_hi[1,2]
  }))

  #TAKE EVERY 3rd POINT BC CAD 18h24
  sset_feasible_scaling <- sset_feasible_scaling[c(1,2,3,4,5)]

  #sanity check plots
  plot(how_muscle_lower_bound_changes, type='l',ylab="M0 in lowest l1 soln", xlab="Task Intensity")
  histogram_muscle_projections(sset_feasible_scaling, range_tension)
  expect_five_points_in_row(sset_feasible_scaling, A_fit$AMatrix)

  ####### Visualize the FAS sets that are produced by feeding in the noiseResponse data.
  rgl.clear()
  num_tasks <- length(sset_feasible_scaling)
  rgl_init(bg = "white")
  extract_3cols <- lapply(sset_feasible_scaling, function(x) x[, c(1, 2, 3)])
  # extract_3cols[[1]] <- NULL #remove the 00000 force
  gradient <- colorRampPalette(c("#a6cee3", "#1f78b4", "#b2df8a", "#fc8d62", "#ffffb3",
    "#bebada"))
  list_of_mats <- add_gradient_to_attrs(extract_3cols, gradient(length(extract_3cols)))
  list_of_mats <- lapply(list_of_mats, function(x) x)

  axes_for_multiple_sets(list_of_mats)
  axes_for_defined_xyz_limits(rep(list(range_tension), 3))
  rgl_convhulls(list_of_mats, points = TRUE)

  ##########PREP CSV MAPS FOR SCALING ###############
  res <- lapply(sset_feasible_scaling, create_and_cbind_map_creation_ids, muscles_of_interest)
  big_har_set_to_test_on_finger <- dcrb(res)
  filename_to_save_har_set <- "scaling_2E_06h56.csv"
  write.csv(big_har_set_to_test_on_finger, to_output_folder(filename_to_save_har_set),
    row.names = FALSE, quote = FALSE)

  #sanity check:
  expect_five_points_in_row_for_csv_maps(filename = filename_to_save_har_set, A_fit=A_fit)




  ############ MANUAL: IDENTIFY TASK MULTIPLIER BOUNDS FOR HORIZONTAL

  # horizontal_line_points <-  tasks_from_flexed_position_via_1A$horizontal_line_points
  # horizontal_line_tasks_from_1A <-  tasks_from_flexed_position_via_1A$horizontal_line_tasks
  # horizontal_line_endpoints_for_2A <- tasks_from_flexed_position_via_1A$line_tasks_for_2A
#figure out the maps that generated the two line endpoints
  pointA <- c(10,10,0,0,10,0,0) #learned from 1A
  pointB <- c(0,0,10,10,0,0,0)
  line_points <- t(t(A_fit$AMatrix) %*% cbind(pointA,pointB))
  horizontal_line_tasks_for_2A <- dcrb(draw_perpendicular_line(line_points[1,1:3],line_points[2,1:3],100))


    num_samples_desired <- 100
    scale_factor_for_magnitude_of_all_horizontal_forces <- 1.5
    colnames(horizontal_line_tasks_for_2A) <- force_names_to_predict[1:3]
    scaled_horizontal_line_tasks <- scale_factor_for_magnitude_of_all_horizontal_forces* horizontal_line_tasks_for_2A
    sset_scaling_horizontal <- multiple_tasks_to_sset(A_fit$AMatrix,scaled_horizontal_line_tasks, thin=100, torque_max_deviation=0.1, num_samples_desired=num_samples_desired)
    sset_feasible_horizontal <- filter_infeasible_tasks(sset_scaling_horizontal, A_fit$AMatrix, max_allowable_residual_from_expected=1e-3, task_bounds = c(scale_factor_for_magnitude_of_all_horizontal_forces, scale_factor_for_magnitude_of_all_horizontal_forces))
    norm_vec(scaled_horizontal_line_tasks[1,])
  #wait, which ones were feasible?
    dcrb(lapply(sset_feasible_horizontal, function(samples){
      boxplot(samples)
      return(attr(samples,'task'))
    }))

    ##########PREP CSV MAPS FOR HORIZONTAL REDIRECTION TASK ###############
    res <- lapply(sset_feasible_horizontal[seq(1,by=16, length.out=5)], create_and_cbind_map_creation_ids, muscles_of_interest)
    big_har_set_to_test_on_finger_horizontal <- dcrb(res)
    filename_for_horizontal <- "horizontal_2F_07h31.csv"
    write.csv(big_har_set_to_test_on_finger_horizontal, to_output_folder(filename_for_horizontal),row.names = FALSE, quote = FALSE)

  #sanity check for horizontal
  expect_five_points_in_row_for_csv_maps(filename = filename_for_horizontal, A_fit=A_fit, muscles_of_interest)



})
test_that("2B looks good",{
  filename_2B <- "noiseResponse2017_12_04_06_13_27_EXMECH_2B_3tap.txt"
})
test_that("2C looks good",{
  filename_2C <- "noiseResponse2017_12_04_06_23_38_EXMECH_2C_3tap.txt"
})
test_that("2D looks good",{
  filename_2D <- "noiseResponse2017_12_04_06_48_43_EXMECH_2D_3tap.txt"
})
test_that("2E looks good",{
  filename_2E <- "noiseResponse2017_12_04_07_09_33_EXMECH_2E_3tap.txt"
})
test_that("2F looks good",{})
######################################################################################
######################################################################################
######################################################################################
######################################################################################

  context("Stage:3 Extended Posture: run tendons against hand, only extensor muscles")
test_that("3A looks good",{
  filename_3A <- "noiseResponse2017_12_04_08_01_31_EXMECH_3A_3tap.txt"
  filepath_3A <- paste0('dec4_extensor_mechanism_input_and_response/1/', filename_3A)


  last_n_milliseconds <- 100
  range_tension <- c(0, 10)
  muscles_of_interest <- muscle_names()[1:5]
  num_muscles <- length(muscles_of_interest)
  force_names_to_predict <- c("JR3_FX","JR3_FY","JR3_FZ","JR3_MX","JR3_MY","JR3_MZ")
  untransformed_noise_response <- as.data.frame(fread(get_Resilio_filepath(filepath_3A)))
  indices_for_null <- 439598:447079
  untransformed_p <- plot_measured_command_reference_over_time(untransformed_noise_response[indices_for_null, ], downsample_amount=1)
  ggsave(to_output_folder(paste0("get_null_indices_via_this_plot_of_untransformed_xray_for_",filename_3A ,".pdf")), untransformed_p, width=90, height=30, limitsize=FALSE)

  noise_response_wo_null <- munge_JR3_data(untransformed_noise_response, input_are_voltages=TRUE, JR3_to_fingertip_distance=0.00956, indices_for_null=indices_for_null)
  p <- plot_measured_command_reference_over_time(noise_response_wo_null)
  ggsave(to_output_folder(paste0("xray_for_",filename_3A ,".pdf")), p, width=90, height=30, limitsize=FALSE)
  noise_hand_responses <- split_by_map_and_remove_wrongly_lengthed_samples(noise_response_wo_null)
  input_output_data <- df_of_hand_response_input_output(noise_hand_responses, last_n_milliseconds)
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

  browser()
  message('Pick 2 points to define the horizontal line endpoints.')

  # this part was done manually, resulting in "tasks_from_flexed_position_via_1A.rds". BC 05h19
  horizontal_line_points <- identify_n_points_from_pointcloud(binary_combination_ffs_points[,1:3],n=2)
  horizontal_line_tasks <- dcrb(draw_perpendicular_line(horizontal_line_points[1,],horizontal_line_points[2,],10))
  spheres3d(horizontal_line_tasks, r=0.10, col="pink")
  message('Pick 1 point to define the scaling direction.')
  task_direction_to_scale <- identify_n_points_from_pointcloud(binary_combination_ffs_points[,1:3],n=1)
  map_that_created_task_dir <- binary_combinations[which(binary_combination_ffs_points[,1]==task_direction_to_scale[1]),]
  line_tasks_for_2A <- as.matrix(t(t(A_fit$AMatrix) %*% t(binary_combinations[c(13,20),])))[,1:3]
  tasks_from_flexed_position <- list(horizontal_line_points=horizontal_line_points, horizontal_line_tasks=horizontal_line_tasks,task_direction_to_scale=task_direction_to_scale,map_that_created_task_dir=map_that_created_task_dir,line_tasks_for_2A=line_tasks_for_2A)
  saveRDS(tasks_from_flexed_position, "tasks_from_flexed_position_via_1A.rds")
  tasks_from_flexed_position_via_1A <- read_rds_from_package_extdata('tasks_from_flexed_position_via_1A.rds')
  horizontal_line_points <-  tasks_from_flexed_position_via_1A$horizontal_line_points
  horizontal_line_tasks <-  tasks_from_flexed_position_via_1A$horizontal_line_tasks
  map_that_created_task_dir <-  tasks_from_flexed_position_via_1A$map_that_created_task_dir

  task_direction_to_scale <-  t(t(A_fit$AMatrix) %*% c( 10, 10, 0, 0, 10))[1:3]
  #Now show the torque FAS just to show what dimensions it is constrained in.
  rgl.init()
  plot_ffs_with_vertices(binary_combination_ffs_points[,4:6], generator_columns_A_matrix[,4:6], alpha_transparency=0.25, range_tension=range_tension)

  ############ MANUAL: IDENTIFY TASK MULTIPLIER BOUNDS FOR SCALING
  task_bounds <- c(1e-2, 1)
  num_samples_desired <- 100
  num_tasks <- 85
  task_multiplier_list <- seq(task_bounds[1], task_bounds[2], length.out = num_tasks)
  task_df <- t(task_direction_to_scale %*% t(task_multiplier_list))
  colnames(task_df) <- force_names_to_predict[1:3]
  sset_scaling <- multiple_tasks_to_sset(A_fit$AMatrix,task_df, thin=100, torque_max_deviation=0.1, num_samples_desired=num_samples_desired)
  sset_feasible_scaling <- filter_infeasible_tasks(sset_scaling, A_fit$AMatrix, max_allowable_residual_from_expected=1e-3, task_bounds=task_bounds)
  force_scaling_tasks <- dcrb(lapply(sset_feasible_scaling, function(samples){
    boxplot(samples)
    return(attr(samples,'task'))
  }))


  l1_cost_limits <- lapply(sset_feasible_scaling, l1_cost_limits)
  how_muscle_lower_bound_changes <- dcc(lapply(l1_cost_limits, function(lo_hi){
    lo_hi[1,2]
  }))

  #TAKE EVERY 3rd POINT BC CAD 18h24
  sset_feasible_scaling <- sset_feasible_scaling[c(8,15,22,29,36)]

  #sanity check plots
  plot(how_muscle_lower_bound_changes, type='l',ylab="M0 in lowest l1 soln", xlab="Task Intensity")
  histogram_muscle_projections(sset_feasible_scaling, range_tension)
  expect_five_points_in_row(sset_feasible_scaling, A_fit$AMatrix)

  ####### Visualize the FAS sets that are produced by feeding in the noiseResponse data.
  rgl.clear()
  num_tasks <- length(sset_feasible_scaling)
  rgl_init(bg = "white")
  extract_3cols <- lapply(sset_feasible_scaling, function(x) x[, c(1, 2, 3)])
  # extract_3cols[[1]] <- NULL #remove the 00000 force
  gradient <- colorRampPalette(c("#a6cee3", "#1f78b4", "#b2df8a", "#fc8d62", "#ffffb3",
    "#bebada"))
  list_of_mats <- add_gradient_to_attrs(extract_3cols, gradient(length(extract_3cols)))
  list_of_mats <- lapply(list_of_mats, function(x) x)

  axes_for_multiple_sets(list_of_mats)
  axes_for_defined_xyz_limits(rep(list(range_tension), 3))
  lapply(list_of_mats, points3d)
  rgl_convhulls(list_of_mats, points = TRUE)
  ##########PREP CSV MAPS FOR SCALING ###############
  res <- lapply(sset_feasible_scaling, create_and_cbind_map_creation_ids, muscles_of_interest)
  big_har_set_to_test_on_finger <- dcrb(res)
  zero_vec_for_flexors <- rep(0, nrow(big_har_set_to_test_on_finger))
  zeros_for_flexors <- data.frame(M5 = zero_vec_for_flexors, M6 = zero_vec_for_flexors)
  har_set_with_zeros_for_flexors <- cbind(big_har_set_to_test_on_finger, zeros_for_flexors)
  filename_to_save_har_set <- "scaling_3E_08h50.csv"
  write.csv(har_set_with_zeros_for_flexors, to_output_folder(filename_to_save_har_set),
    row.names = FALSE, quote = FALSE)

  #sanity check:
  expect_five_points_in_row_for_csv_maps(filename = filename_to_save_har_set, A_fit=A_fit,muscles_of_interest=muscles_of_interest)




  ############ MANUAL: IDENTIFY TASK MULTIPLIER BOUNDS FOR HORIZONTAL

  spheres3d(horizontal_line_tasks, r=0.05,col="pink")
    num_samples_desired <- 100
    scale_factor_for_magnitude_of_all_horizontal_forces <- .80
    colnames(horizontal_line_tasks) <- force_names_to_predict[1:3]
    scaled_horizontal_line_tasks <- scale_factor_for_magnitude_of_all_horizontal_forces* horizontal_line_tasks
    sset_scaling_horizontal <- multiple_tasks_to_sset(A_fit$AMatrix,scaled_horizontal_line_tasks, thin=100, torque_max_deviation=0.1, num_samples_desired=num_samples_desired)
    sset_feasible_horizontal <- filter_infeasible_tasks(sset_scaling_horizontal, A_fit$AMatrix, max_allowable_residual_from_expected=1e-3, task_bounds = c(scale_factor_for_magnitude_of_all_horizontal_forces, scale_factor_for_magnitude_of_all_horizontal_forces))

  #wait, which ones were feasible?
    dcrb(lapply(sset_feasible_horizontal, function(samples){
      boxplot(samples)
      return(attr(samples,'task'))
    }))

    ##########PREP CSV MAPS FOR HORIZONTAL REDIRECTION TASK ###############
    res <- lapply(sset_feasible_horizontal[c(3,4,5,6,7)], create_and_cbind_map_creation_ids, muscles_of_interest)
    big_har_set_to_test_on_finger_horizontal <- dcrb(res)
    har_set_with_zeros_for_flexors <- cbind(big_har_set_to_test_on_finger_horizontal, zeros_for_flexors)
    filename_for_horizontal <- "horizontal_3F_09h33.csv"
    write.csv(har_set_with_zeros_for_flexors, to_output_folder(filename_for_horizontal),row.names = FALSE, quote = FALSE)

  #sanity check for horizontal
  expect_five_points_in_row_for_csv_maps(filename = filename_for_horizontal, A_fit=A_fit, muscles_of_interest)





})
test_that("3B looks good",{})
test_that("3C looks good",{})
test_that("3D looks good",{})
test_that("3E looks good",{})
test_that("3F looks good",{})
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
  context("Stage:4 Extended Posture: run tendons against hand, all muscles, using task for stage 3")
test_that("4A looks good",{

  filename_4A <- "noiseResponse2017_12_04_09_49_10_EXMECH_4A_3tap.txt"


    last_n_milliseconds <- 100
    range_tension <- c(0, 10)
    muscles_of_interest <- muscle_names()
    num_muscles <- length(muscles_of_interest)
    force_names_to_predict <- c("JR3_FX","JR3_FY","JR3_FZ","JR3_MX","JR3_MY","JR3_MZ")
    untransformed_noise_response <- as.data.frame(fread(get_Resilio_filepath(paste0("dec4_extensor_mechanism_input_and_response/4/",filename_4A))))
    indices_for_null <- 457320:461318
    untransformed_p <- plot_measured_command_reference_over_time(untransformed_noise_response[indices_for_null,])
    ggsave(to_output_folder(paste0("get_null_indices_via_this_plot_of_untransformed_xray_for_",filename_4A ,".pdf")), untransformed_p, width=90, height=30, limitsize=FALSE)
    # colMeans(untransformed_noise_response[indices_for_null,]) - JR3_null_from_1A # shows few differences in JR3 sensor vals over time
    noise_response_wo_null <- munge_JR3_data(untransformed_noise_response, input_are_voltages=TRUE, JR3_to_fingertip_distance=0.00956, indices_for_null=indices_for_null)
    p <- plot_measured_command_reference_over_time(noise_response_wo_null)
    ggsave(to_output_folder(paste0("xray_for_",filename_4A ,".pdf")), p, width=90, height=30, limitsize=FALSE)
    noise_hand_responses <- split_by_map_and_remove_wrongly_lengthed_samples(noise_response_wo_null)
    input_output_data <- df_of_hand_response_input_output(noise_hand_responses, last_n_milliseconds)
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
  ##################################################
  ##################################################
  #BRING IN TASKS FROM 1A and create scaling, horizontal.
 vertex_generated_scaling_task_direction<- t(t(A_fit$AMatrix) %*% c( 10, 10, 0, 0, 10,0,0))[,1:3]

    tasks_from_flexed_position_via_1A <- read_rds_from_package_extdata("tasks_from_flexed_position_via_1A.rds")
    task_direction_to_scale <-  tasks_from_flexed_position_via_1A$task_direction_to_scale
    map_that_created_task_dir <-  tasks_from_flexed_position_via_1A$map_that_created_task_dir

    task_direction_to_scale <- as.numeric(t(t(A_fit$AMatrix) %*% c(map_that_created_task_dir,0,0))[1:3])
    names(task_direction_to_scale) <- c("JR3_FX","JR3_FY", "JR3_FZ")

    #Now show the torque FAS just to show what dimensions it is constrained in.
    rgl.init()
    plot_ffs_with_vertices(binary_combination_ffs_points[,4:6], generator_columns_A_matrix[,4:6], alpha_transparency=0.25, range_tension=range_tension)

    ############ MANUAL: IDENTIFY TASK MULTIPLIER BOUNDS FOR SCALING
    task_bounds <- c(1e-2, 7)
    num_samples_desired <- 100
    num_tasks <- 1000
    task_multiplier_list <- seq(task_bounds[1], task_bounds[2], length.out = num_tasks)
    task_df <- t(vertex_generated_scaling_task_direction %*% t(task_multiplier_list))
    colnames(task_df) <- force_names_to_predict[1:3]
    sset_scaling <- multiple_tasks_to_sset(A_fit$AMatrix,task_df, thin=100, torque_max_deviation=0.05, num_samples_desired=num_samples_desired)
    sset_feasible_scaling <- filter_infeasible_tasks(sset_scaling, A_fit$AMatrix, max_allowable_residual_from_expected=1e-3, task_bounds=task_bounds)
    dcrb(lapply(sset_feasible_scaling, function(samples){
      boxplot(samples)
      return(attr(samples,'task'))
    }))

    l1_cost_limits <- lapply(sset_feasible_scaling, l1_cost_limits)
    how_muscle_lower_bound_changes <- dcc(lapply(l1_cost_limits, function(lo_hi){
      lo_hi[1,2]
    }))

    #TAKE EVERY 14th POINT BC CAD 10h26 dec4
    sset_feasible_scaling <- sset_feasible_scaling[c(1,15,29,43,57)]

    #sanity check plots
    plot(how_muscle_lower_bound_changes, type='l',ylab="M0 in lowest l1 soln", xlab="Task Intensity")
    histogram_muscle_projections(sset_feasible_scaling, range_tension)
    expect_five_points_in_row(sset_feasible_scaling, A_fit$AMatrix)

    ####### Visualize the FAS sets that are produced by feeding in the noiseResponse data.
    rgl.clear()
    num_tasks <- length(sset_feasible_scaling)
    rgl_init(bg = "white")
    extract_3cols <- lapply(sset_feasible_scaling, function(x) x[, c(1, 2, 3)])
    # extract_3cols[[1]] <- NULL #remove the 00000 force
    gradient <- colorRampPalette(c("#a6cee3", "#1f78b4", "#b2df8a", "#fc8d62", "#ffffb3",
      "#bebada"))
    list_of_mats <- add_gradient_to_attrs(extract_3cols, gradient(length(extract_3cols)))
    list_of_mats <- lapply(list_of_mats, function(x) x)

    axes_for_multiple_sets(list_of_mats)
    axes_for_defined_xyz_limits(rep(list(range_tension), 3))
    rgl_convhulls(list_of_mats, points = TRUE)

    ##########PREP CSV MAPS FOR SCALING ###############
    res <- lapply(sset_feasible_scaling, create_and_cbind_map_creation_ids, muscles_of_interest)
    big_har_set_to_test_on_finger <- dcrb(res)
    filename_to_save_har_set <- "scaling_4E_10h27.csv"
    write.csv(big_har_set_to_test_on_finger, to_output_folder(filename_to_save_har_set),
      row.names = FALSE, quote = FALSE)

    #sanity check:
    expect_five_points_in_row_for_csv_maps(filename = filename_to_save_har_set, A_fit=A_fit)




    ############ MANUAL: IDENTIFY TASK MULTIPLIER BOUNDS FOR HORIZONTAL

    # horizontal_line_points <-  tasks_from_flexed_position_via_1A$horizontal_line_points
    # horizontal_line_tasks_from_1A <-  tasks_from_flexed_position_via_1A$horizontal_line_tasks
    # horizontal_line_endpoints_for_2A <- tasks_from_flexed_position_via_1A$line_tasks_for_2A
  #figure out the maps that generated the two line endpoints

  pointA <- c(10,10,0,0,10,0,0) #learned from 1A
  pointB <- c(0,0,10,10,0,0,0)

    line_points <- t(t(A_fit$AMatrix) %*% cbind(pointA,pointB))
    horizontal_line_tasks_for_2A <- dcrb(draw_perpendicular_line(line_points[1,1:3],line_points[2,1:3],1000))

#couldn't use the line points. no feasible solutions, and any ones that were feaible only spanned a 0.04N space. Not useful to our study.
horizontal_line_points <- identify_n_points_from_pointcloud(binary_combination_ffs_points[,1:3],n=2)
horizontal_line_tasks <- dcrb(draw_perpendicular_line(horizontal_line_points[1,],horizontal_line_points[2,],10))
spheres3d(horizontal_line_tasks, r=0.10, col="pink")


      num_samples_desired <- 100
      scale_factor_for_magnitude_of_all_horizontal_forces <- 0.8
      colnames(horizontal_line_tasks) <- force_names_to_predict[1:3]
      scaled_horizontal_line_tasks <- scale_factor_for_magnitude_of_all_horizontal_forces* horizontal_line_tasks
      sset_scaling_horizontal <- multiple_tasks_to_sset(A_fit$AMatrix,scaled_horizontal_line_tasks, thin=100, torque_max_deviation=0.2, num_samples_desired=num_samples_desired)
      sset_feasible_horizontal <- filter_infeasible_tasks(sset_scaling_horizontal, A_fit$AMatrix, max_allowable_residual_from_expected=1e-3, task_bounds = c(scale_factor_for_magnitude_of_all_horizontal_forces, scale_factor_for_magnitude_of_all_horizontal_forces))
      norm_vec(scaled_horizontal_line_tasks[1,])
    #wait, which ones were feasible?
      dcrb(lapply(sset_feasible_horizontal, function(samples){
        boxplot(samples)
        return(attr(samples,'task'))
      }))

      ##########PREP CSV MAPS FOR HORIZONTAL REDIRECTION TASK ###############
      res <- lapply(sset_feasible_horizontal[c(2,4,6,8,10)], create_and_cbind_map_creation_ids, muscles_of_interest)
      big_har_set_to_test_on_finger_horizontal <- dcrb(res)
      filename_for_horizontal <- "horizontal_4F_10h38.csv"
      write.csv(big_har_set_to_test_on_finger_horizontal, to_output_folder(filename_for_horizontal),row.names = FALSE, quote = FALSE)
    #sanity check for horizontal
    expect_five_points_in_row_for_csv_maps(filename = filename_for_horizontal, A_fit=A_fit, muscles_of_interest)


})
test_that("4B looks good",{})
test_that("4C looks good",{})
test_that("4D looks good",{})
test_that("4E looks good",{})
test_that("4F looks good",{})
