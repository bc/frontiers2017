context("Testing Nov 10 Data")
map_id_table <- fread(get_Resilio_filepath("map_unit_cube_nov11.csv"))
filenames <- c("noPostureNeutralForceTrials2017_11_12_14_53_25.txt", "noPostureNeutralForceTrials2017_11_12_14_50_59.txt",
  "noPostureNeutralForceTrials2017_11_12_14_48_27.txt", "noPostureNeutralForceTrials2017_11_12_14_46_00.txt",
  "noPostureNeutralForceTrials2017_11_12_14_43_47.txt")
filepaths <- dcc(lapply(filenames, get_Resilio_filepath))

experiments <- lapply(filepaths, function(file) {
  fread(file)
})


sample_maps_data <- as.data.frame(experiments[[1]])
JR3_sensor_null <- colMeans(head(sample_maps_data, 100))
sample_maps_data <- zero_out_JR3_sensors(sample_maps_data, JR3_sensor_null)
p <- ggplot(data = head(sample_maps_data, 100))
p <- p + geom_line(aes(time, JR3_FX))
p <- p + geom_line(aes(time, JR3_FY))
p <- p + geom_line(aes(time, JR3_FZ))
p <- p + geom_line(aes(time, measured_M0))
p

dts <- split(sample_maps_data, sample_maps_data$map_creation_id)
are_correct_length <- dcc(lapply(dts, function(dt) {
  return(nrow(dt) >= 700 && nrow(dt) < 805)
}))

maps <- dts[are_correct_length]
input_output_data <- dcrb(lapply(lapply(maps, tail, 100), colMeans))

data <- df_split_into_training_and_testing(input_output_data, fraction_training = 0.8)
training_data <- data$train
test_data <- data$test

force_names_to_predict <- c("JR3_FX","JR3_FY", "JR3_FZ")
num_muscles <- 7
A_fit <- find_A_matrix_without_offset(as.data.frame(training_data), reference(muscle_names()), force_names_to_predict)
fit_evaluation_without_offset(A_fit, as.data.frame(test_data))

range_tension <- c(3.1, 10.1)
AMatrix_with_offset <- A_fit$AMatrix
muscle_constraints_matrix <- diag(rep(1, num_muscles))
generator_columns_A_matrix <- t(A_fit$AMatrix)
dim(generator_columns_A_matrix)
dim(muscle_constraints_matrix)

task_multiplier <- c(0.9,2.8)
task_force <- c(0.33152926, 0.07102741, 0.22046813)
sset <- lapply(seq(0.9,2.8, length.out=10), function(i) {

  one_observed_force <- task_force*i
  big_A <- rbind(generator_columns_A_matrix,
    -muscle_constraints_matrix, muscle_constraints_matrix)
    big_b <- as.numeric(c(one_observed_force, rep(-range_tension[1],
      num_muscles), rep(range_tension[2], num_muscles)))

      dir_vector <- c(rep("=", length(one_observed_force)),
      rep("<=", num_muscles),
      rep("<=", num_muscles)
    )
    constr <- list(constr = big_A, dir = dir_vector, rhs = big_b)
    constraints_are_feasible(constr)
    state <- har.init(constr, thin = 100)
    result <- har.run(state, n.samples = 100)
    samples <- result$samples
    #Try running those samples back through the A_matrix
    t(A_fit$AMatrix) %*% t(samples)

    #Show histograms of the FAS
    par(mfrow=c(1,7))
    lapply(1:7, function(muscle_num){
      hist(samples[,muscle_num], breaks=10, main =paste("M",i, "at", i, collapse=""), xlab="Tendon force (N)", xlim=c(0,11))
    })


    lowest_l1_cost_soln <- samples[which.min(rowSums(samples)), ]
    highest_l1_cost_soln <- samples[which.max(rowSums(samples)), ]

    message('lowest l1 cost solution:')
    message( format(lowest_l1_cost_soln,digits=2))
    message('highest l1 cost solution:')
    message( format(highest_l1_cost_soln,digits=2) )
    test_predicted_response <- as.matrix(samples %*% A_fit$AMatrix)
    boxplot(test_predicted_response, ylab = "Tension N for FX,FY,FZ, Torque Nm for MX,MY,MZ", main="what do most of the FAS-sampled forces product in output space? ")
    plot3d(test_predicted_response)
    #TODO get the test data from the actual data collected
    # test_observed_response <- test_data[force_column_names]
    # res_test <- test_observed_response - test_predicted_response
    # summary(res_test)


    # parcoord(samples)
    rgl.clear()
    plot3d(samples, xlim=c(0,11),ylim=c(0,11),zlim=c(0,11))  #show 3d plane
    Sys.sleep(0.5)
    return(samples)
})
    num_tasks <- length(sset)
    rgl_init(bg = "white")
    extract_3cols <- lapply(sset[c(1,2,5,8,10)], function(df) df[,5:7])
    gradient <- colorRampPalette(c("#a6cee3", "#1f78b4", "#b2df8a", "#fc8d62", "#ffffb3",
      "#bebada"))
    list_of_mats <- add_gradient_to_attrs(extract_3cols,gradient(length(extract_3cols)))

    axes_for_multiple_sets(list_of_mats)
    # Add x, y, and z Axes
    lapply(list_of_mats, function(mat) {
      xyz_points_with_convhull(mat, col = attr(mat, "color"), points = FALSE)
    })





  res <- lapply(sset, function(samples) {
    cbound <- cbind(generate_map_creation_ids(nrow(samples)), as.data.frame(samples))
    colnames(cbound) <- c("map_creation_id", muscle_names())
    return(cbound)
  })

big_har_set_to_test_on_finger <- dcrb(res)

write.csv(big_har_set_to_test_on_finger, "scaling_task_n100_per_outputvec_of_interest_10_steps.csv", row.names=FALSE,quote=FALSE)
#make a little db to remember which map was trying to achieve which task.
tasklists <- lapply(seq(0.9,2.8, length.out=10), function(x) {
  dcrb(rep(list(x*c(0.33152926, 0.07102741, 0.22046813)),100))
})
task_list_df <- dcrb(tasklists)
colnames(task_list_df) <- force_names_to_predict
maps_with_target_tasks <- cbind(big_har_set_to_test_on_finger,task_list_df)

##TODO GET data from the cadaver finger from big_har_set_to_test_on_finger
scaling <- as.data.frame(fread(get_Resilio_filepath("noPostureNeutralForceTrials2017_11_14_12_46_57.txt")))
map_id_table_scaling <- as.data.frame(fread(get_Resilio_filepath("scaling_task_n100_per_outputvec_of_interest_10_steps_nov14.csv")))

JR3_sensor_null <- colMeans(head(scaling, 30))
#TODO test zero out sensors
sample_maps_data_scaling <- zero_out_JR3_sensors(scaling, JR3_sensor_null)
p <- ggplot(data = head(sample_maps_data_scaling, 30))
p <- p + geom_line(aes(time, JR3_FX))
p <- p + geom_line(aes(time, JR3_FY))
p <- p + geom_line(aes(time, JR3_FZ))
p

dts_scaling <- split(sample_maps_data_scaling, sample_maps_data_scaling$map_creation_id)
are_correct_length <- dcc(lapply(dts, function(dt) {
  return(nrow(dt) >= 700 && nrow(dt) < 805)
}))


maps_scaling <- dts[are_correct_length]
scaling_input_output_data <- as.data.frame(dcrb(lapply(lapply(maps, tail, 100), colMeans)))
maps_with_target_tasks$map_creation_id <- as.numeric(as.character(maps_with_target_tasks$map_creation_id))
a <- merge(scaling_input_output_data,maps_with_target_tasks,by="map_creation_id")



## later, do some more stuff with replicates
