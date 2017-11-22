context("test_response_to_noise_with_replicates.r")
noise_datapaths <- c("noiseTrial2017_11_19_18_07_57.txt",
"noiseTrial2017_11_19_18_03_36.txt",
"noiseTrial2017_11_19_17_59_18.txt",
"noiseTrial2017_11_19_17_54_54.txt",
"noiseTrial2017_11_19_17_49_22.txt",
"noiseTrial2017_11_19_17_45_02.txt",
"noiseTrial2017_11_19_17_40_37.txt",
"noiseTrial2017_11_19_17_36_16.txt",
"noiseTrial2017_11_19_17_31_56.txt")


sample_maps_data <- as.data.frame(fread(get_Resilio_filepath(noise_datapaths[[2]])))
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
replicate_list <- lapply(dts, split_by_replicate)
sd_of_replicates <- dcrb(lapply(replicate_list, column_sd_across_replicates))
boxplot(sd_of_replicates[,measured(muscle_names())], xlab="Muscle", ylab="SD (N)")
boxplot(sd_of_replicates[,command(muscle_names())], xlab="Muscle", ylab="SD voltage?")
boxplot(sd_of_replicates[,dots_to_underscores(force_column_names)], xlab="Muscle", ylab="SD in Force (N)")
