

context('hand 3 dec 20')
cat <- read.csv("/Users/briancohn/Resilio\ Sync/data/dec20BC1/dec20_PD_EXTMECH/big_jumbo_set_for_posture_dependence_and_extmech_914_NFORCES.csv")[1:350,]
parallel_300 <- cat[1:300,]
replicate_50 <- cat[301:350,]
signals_prefix <- "get_null_indices_via_this_plot_of_untransformed_xray_for_"

#Define dec20 hand3 parameters
JR3_to_fingertip_distance <- 0.00802 #about 9mm in meters TODO
last_n_milliseconds <- 100
range_tension <- c(0, 10)
muscles_of_interest <- muscle_names()
num_muscles <- length(muscles_of_interest)
force_names_to_predict <- c("JR3_FX","JR3_FY","JR3_FZ","JR3_MX","JR3_MY","JR3_MZ")

#Define maps of interest to extract into lists
maps_of_interest <- parallel_300[,muscle_names()]
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

  group_indices <- list(lower=2,
                        upper=301)
  expect_true(all(maps_match_across_M0_and_map_groups(noise_response_wo_null, group_indices=group_indices, maps_of_interest=maps_of_interest)))
  tails <- extract_trial_tails_by_map_group_indices(noise_response_wo_null, group_indices, 100)
  input_output_data <- dcrb(lapply(tails,colMeans))
  expect_equal(nrow(input_output_data), 300)
  expect_equal(ncol(input_output_data), 37)
  write.csv(input_output_data,to_output_folder('hand3_ultraflex_clean.csv'), row.names=FALSE)
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

  group_indices <- list(lower=2,
                        upper=301)
  expect_true(all(maps_match_across_M0_and_map_groups(noise_response_wo_null, group_indices=group_indices, maps_of_interest=maps_of_interest)))
  tails <- extract_trial_tails_by_map_group_indices(noise_response_wo_null, group_indices, 100)
  input_output_data <- dcrb(lapply(tails,colMeans))
  expect_equal(nrow(input_output_data), 300)
  expect_equal(ncol(input_output_data), 37)
  write.csv(input_output_data,to_output_folder('hand3_flex_clean.csv'), row.names=FALSE)
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

  group_indices <- list(lower=2,
                        upper=301)
  expect_true(all(maps_match_across_M0_and_map_groups(noise_response_wo_null, group_indices=group_indices, maps_of_interest=maps_of_interest)))
  tails <- extract_trial_tails_by_map_group_indices(noise_response_wo_null, group_indices, 100)
  input_output_data <- dcrb(lapply(tails,colMeans))
  expect_equal(nrow(input_output_data), 300)
  expect_equal(ncol(input_output_data), 37)
  write.csv(input_output_data,to_output_folder('hand3_extend_clean.csv'), row.names=FALSE)
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

  group_indices <- list(lower=2,
                        upper=301)
  expect_true(all(maps_match_across_M0_and_map_groups(noise_response_wo_null, group_indices=group_indices, maps_of_interest=maps_of_interest)))
  tails <- extract_trial_tails_by_map_group_indices(noise_response_wo_null, group_indices, 100)
  input_output_data <- dcrb(lapply(tails,colMeans))
  expect_equal(nrow(input_output_data), 300)
  expect_equal(ncol(input_output_data), 37)
  write.csv(input_output_data,to_output_folder('hand3_ultraextend_clean.csv'), row.names=FALSE)
})
