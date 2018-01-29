context('hand 3 dec 20')
# cumsum: 300 350 420 720 770 854 914
sections_of_interest <- dec20_PD_EXTMECH_maps_of_interest_by_section()

signals_prefix <- "get_null_indices_via_this_plot_of_untransformed_xray_for_"

#Define dec20 hand3 parameters
JR3_to_fingertip_distance <- 0.00802 #about 9mm in meters TODO
last_n_milliseconds <- 100
range_tension <- c(0, 10)
muscles_of_interest <- muscle_names()
num_muscles <- length(muscles_of_interest)
force_names_to_predict <- c("JR3_FX","JR3_FY","JR3_FZ","JR3_MX","JR3_MY","JR3_MZ")
group_indices <- list(lower=2,
                      upper=301)
#Define maps of interest to extract into lists
maps_of_interest <- sections_of_interest$parallel[,muscle_names()]
colnames(maps_of_interest) <- reference(muscle_names())

test_that("hand 3 ultraflex", {

filename_3A <- "noiseResponse_ST1BC_2017_12_20_19_50_38_PD_Extmech_good_ultraflex_NOTAP.txt"
hand3_dec20_ultraflex <- fread_df_from_Resilio(filename_3A)
manual_3tap_for_hand3_ultraflex <- fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_20_20_05_37_manual_3tap_extmech_for_ultraflex.txt")
JR3_sensor_null <- colMeans(manual_3tap_for_hand3_ultraflex[15000:20000,])

  untransformed_p <- plot_measured_command_reference_over_time(hand3_dec20_ultraflex)
  ggsave(to_output_folder(paste0(signals_prefix,filename_3A ,".pdf")),
   untransformed_p, width=90, height=30, limitsize=FALSE)
  noise_response_wo_null <- munge_JR3_data(hand3_dec20_ultraflex,remove_nonzero_map_creation_ids=FALSE, input_are_voltages=TRUE, JR3_to_fingertip_distance=JR3_to_fingertip_distance,JR3_sensor_null=JR3_sensor_null)
  p <- plot_measured_command_reference_over_time(noise_response_wo_null)
  ggsave(to_output_folder(paste0("xray_for_",filename_3A ,".pdf")), p, width=90, height=30, limitsize=FALSE)
  expect_true(all(maps_match_across_M0_and_map_groups(noise_response_wo_null, group_indices=group_indices, maps_of_interest=maps_of_interest)))
  response <-  extract_static_and_dynamic_data(noise_response_wo_null, group_indices, last_n_milliseconds)
  write_csv_of_timeseries_and_input_output(dcrb(response$dynamic_trials_list),response$static_df,'hand3_ultraflex',last_n_milliseconds)


})

test_that("hand 3 flex", {
filename_3B <- "noiseResponse_ST1BC_2017_12_20_19_20_56_PD_extmech_good_45_45_10.txt"
hand3_dec20_flex <- fread_df_from_Resilio(filename_3B)
indices_for_null <- 1000000:1100000
  untransformed_p <- plot_measured_command_reference_over_time(hand3_dec20_flex[indices_for_null,])
  ggsave(to_output_folder(paste0(signals_prefix,filename_3B ,".pdf")),
   untransformed_p, width=90, height=30, limitsize=FALSE)
  noise_response_wo_null <- munge_JR3_data(hand3_dec20_flex,remove_nonzero_map_creation_ids=FALSE, input_are_voltages=TRUE, indices_for_null=indices_for_null, JR3_to_fingertip_distance=JR3_to_fingertip_distance,JR3_sensor_null=NULL)
  p <- plot_measured_command_reference_over_time(noise_response_wo_null)
  ggsave(to_output_folder(paste0("xray_for_",filename_3B ,".pdf")), p, width=90, height=30, limitsize=FALSE)
  expect_true(all(maps_match_across_M0_and_map_groups(noise_response_wo_null, group_indices=group_indices, maps_of_interest=maps_of_interest)))
  response <-  extract_static_and_dynamic_data(noise_response_wo_null, group_indices, last_n_milliseconds)
  write_csv_of_timeseries_and_input_output(dcrb(response$dynamic_trials_list),response$static_df,'hand3_flex',last_n_milliseconds)
})
test_that("hand 3 extend", {

filename_3C <- "noiseResponse_ST1BC_2017_12_20_20_16_25_3tap_all_good_extended_ish_posture.txt"
hand3_dec20_extend <- fread_df_from_Resilio(filename_3C)
indices_for_null <- 759096:763441
  untransformed_p <- plot_measured_command_reference_over_time(hand3_dec20_extend[indices_for_null,])
  ggsave(to_output_folder(paste0(signals_prefix,filename_3C ,".pdf")),
   untransformed_p, width=90, height=30, limitsize=FALSE)
  noise_response_wo_null <- munge_JR3_data(hand3_dec20_extend,remove_nonzero_map_creation_ids=FALSE, input_are_voltages=TRUE, indices_for_null=indices_for_null, JR3_to_fingertip_distance=JR3_to_fingertip_distance,JR3_sensor_null=NULL)
  p <- plot_measured_command_reference_over_time(noise_response_wo_null)
  ggsave(to_output_folder(paste0("xray_for_",filename_3C ,".pdf")), p, width=90, height=30, limitsize=FALSE)
  expect_true(all(maps_match_across_M0_and_map_groups(noise_response_wo_null, group_indices=group_indices, maps_of_interest=maps_of_interest)))
  response <-  extract_static_and_dynamic_data(noise_response_wo_null, group_indices, last_n_milliseconds)
  write_csv_of_timeseries_and_input_output(dcrb(response$dynamic_trials_list),response$static_df,'hand3_extend',last_n_milliseconds)
})


test_that("hand 3 ultraextended", {

filename_3D <- "noiseResponse_ST1BC_2017_12_20_20_44_47_3tap_all_good_10_10_10_extended_posture.txt"
hand3_dec20_ultraextend <- fread_df_from_Resilio(filename_3D)
indices_for_null <-  761547:nrow(hand3_dec20_ultraextend)
  untransformed_p <- plot_measured_command_reference_over_time(hand3_dec20_ultraextend[indices_for_null,])
  ggsave(to_output_folder(paste0(signals_prefix,filename_3D ,".pdf")),
   untransformed_p, width=90, height=30, limitsize=FALSE)
  noise_response_wo_null <- munge_JR3_data(hand3_dec20_ultraextend,remove_nonzero_map_creation_ids=FALSE, input_are_voltages=TRUE, indices_for_null=indices_for_null, JR3_to_fingertip_distance=JR3_to_fingertip_distance,JR3_sensor_null=NULL)
  p <- plot_measured_command_reference_over_time(noise_response_wo_null)
  ggsave(to_output_folder(paste0("xray_for_",filename_3D ,".pdf")), p, width=90, height=30, limitsize=FALSE)
  expect_true(all(maps_match_across_M0_and_map_groups(noise_response_wo_null, group_indices=group_indices, maps_of_interest=maps_of_interest)))
  response <-  extract_static_and_dynamic_data(noise_response_wo_null, group_indices, last_n_milliseconds)
  write_csv_of_timeseries_and_input_output(dcrb(response$dynamic_trials_list),response$static_df,'hand3_ultraextend',last_n_milliseconds)
})


test_that("hand 3 ultraflex REPLICATES", {

filename_3A <- "noiseResponse_ST1BC_2017_12_20_19_50_38_PD_Extmech_good_ultraflex_NOTAP.txt"
hand3_dec20_ultraflex <- fread_df_from_Resilio(filename_3A)
manual_3tap_for_hand3_ultraflex <- fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_20_20_05_37_manual_3tap_extmech_for_ultraflex.txt")
JR3_sensor_null <- colMeans(manual_3tap_for_hand3_ultraflex[15000:20000,])
  untransformed_p <- plot_measured_command_reference_over_time(hand3_dec20_ultraflex)
  ggsave(to_output_folder(paste0(signals_prefix,filename_3A ,".pdf")),
   untransformed_p, width=90, height=30, limitsize=FALSE)
  noise_response_wo_null <- munge_JR3_data(hand3_dec20_ultraflex,remove_nonzero_map_creation_ids=FALSE, input_are_voltages=TRUE, JR3_to_fingertip_distance=JR3_to_fingertip_distance,JR3_sensor_null=JR3_sensor_null)
  p <- plot_measured_command_reference_over_time(noise_response_wo_null)
  ggsave(to_output_folder(paste0("xray_for_",filename_3A ,".pdf")), p, width=90, height=30, limitsize=FALSE)
  expect_true(all(maps_match_across_M0_and_map_groups(noise_response_wo_null, group_indices=group_indices, maps_of_interest=maps_of_interest)))
  response <-  extract_static_and_dynamic_data(noise_response_wo_null, group_indices = list(lower=302,upper=352),last_n_milliseconds=100)
  tails <- extract_tails_from_trials(response[[1]],last_n_milliseconds)
  input_output_data <- as.data.frame(dcrb(lapply(tails,colMeans))[1:32,])
 sorted_input_output <- data.table(input_output_data[order(input_output_data$reference_M0),])
 a<- sorted_input_output[, , by=reference_M0]
 #n = 5 replicates per map
 list_of_replicate_results <-  lapply(split(sorted_input_output, sorted_input_output$reference_M0), tail, 5)
 iqr_set <- lapply(list_of_replicate_results, function(x) {
apply(as.data.frame(x)[,force_names_to_predict],2,IQR)
})
 names(iqr_set) <- 0:4
 df_iqr_set <- dcrb(iqr_set)
 xtable(df_iqr_set) #use in latex as replicability table

 input_replicate_maps <-  as.data.frame(dcrb(lapply(split(sorted_input_output, sorted_input_output$reference_M0), tail, 1)))[,reference(muscle_names())]
 colnames(input_replicate_maps) <- muscle_names()
 rownames(input_replicate_maps) <- 0:4
})
