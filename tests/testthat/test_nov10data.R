context("Testing Nov 10 Data")
map_id_table <- fread(get_Resilio_filepath("map_unit_cube_nov11.csv"))
filenames <- c("noPostureNeutralForceTrials2017_11_12_14_53_25.txt", "noPostureNeutralForceTrials2017_11_12_14_50_59.txt",
  "noPostureNeutralForceTrials2017_11_12_14_48_27.txt", "noPostureNeutralForceTrials2017_11_12_14_46_00.txt",
  "noPostureNeutralForceTrials2017_11_12_14_43_47.txt")
filepaths <- dcc(lapply(filenames, get_Resilio_filepath))

experiments <- lapply(filepaths, function(file) {
  fread(file)
})

##' Zero out JR3 sensors
##' @param df dataframe of raw timeseries data including JR3_FX, etc.
##' @param JR3_sensor_null vecotr of 6 values representing the mean in the first 100ms
zero_out_JR3_sensors <- function(df, JR3_sensor_null) {
  for (i in c("JR3_FX", "JR3_FY", "JR3_FZ", "JR3_MX", "JR3_MY", "JR3_MZ")) {
    df[,i] <- df[,i] - JR3_sensor_null[[i]]
  }
    return(df)
  }

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


num_muscles <- 7
A_fit <- find_A_matrix_without_offset(as.data.frame(training_data), reference(muscle_names()), c("JR3_FX","JR3_FY", "JR3_FZ"))
fit_evaluation_without_offset(A_fit, as.data.frame(test_data))

range_tension <- c(3.1, 10.1)
AMatrix_with_offset <- A_fit$AMatrix
muscle_constraints_matrix <- diag(rep(1, num_muscles))
generator_columns_A_matrix <- t(A_fit$AMa
  trix)
dim(generator_columns_A_matrix)
dim(muscle_constraints_matrix)
big_A <- rbind(generator_columns_A_matrix,
  -muscle_constraints_matrix, muscle_constraints_matrix)
one_observed_force <- c(0.33152926, 0.07102741, 0.22046813)
big_b <- as.numeric(c(one_observed_force, rep(-range_tension[1],
  num_muscles), rep(range_tension[2], num_muscles)))

dir_vector <- c(rep("=", length(one_observed_force)),
                rep("<=", num_muscles),
                rep("<=", num_muscles)
              )
constr <- list(constr = big_A, dir = dir_vector, rhs = big_b)
constraints_are_feasible(constr)
state <- har.init(constr)
result <- har.run(state, n.samples = 100000)
samples <- result$samples
#Try running those samples back through the A_matrix
t(A_fit$AMatrix) %*% t(samples)
#Show histograms of the FAS
par(mfrow=c(1,7))
lapply(1:7, function(i){
  hist(samples[,i], breaks=1000, main =paste("M",i), xlab="Tendon force (N)")
})
#Show parcoord of the FAS
par(mfrow=c(1,1))
parcoord(head(samples,200))











## later, do some more stuff with replicates
p <- experiments[[1]]
muscle_names_of_interest <- colnames(map_id_table)[2:ncol(map_id_table)]
only_muscle_reference_vals <- as.data.frame(map_id_table)[, muscle_names_of_interest]
initialization_row <- t(data.frame(rep(3, ncol(map_id_table) - 1)))
colnames(initialization_row) <- dcc(lapply(muscle_names_of_interest, paste0, "_initial"))
rownames(initialization_row) <- NULL
cbind(only_muscle_reference_vals)

for (i in 1:nrow(map_id_table)) {
}
test_that("load_")
