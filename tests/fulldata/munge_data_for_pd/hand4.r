context('hand 4 dec 20')
cat <- read.csv("/Users/briancohn/Resilio\ Sync/data/dec20BC1/dec20_PD_EXTMECH/big_jumbo_set_for_posture_dependence_and_extmech_914_NFORCES.csv")[1:350,]
parallel_300 <- cat[1:300,]
replicate_50 <- cat[301:350,]
signals_prefix <- "get_null_indices_via_this_plot_of_untransformed_xray_for_"

#Define dec20 hand4 parameters
JR3_to_fingertip_distance <- 0.00802 #about 9mm in meters. Same for both hands.
last_n_milliseconds <- 100
range_tension <- c(0, 10)
muscles_of_interest <- muscle_names()
num_muscles <- length(muscles_of_interest)
force_names_to_predict <- c("JR3_FX","JR3_FY","JR3_FZ","JR3_MX","JR3_MY","JR3_MZ")

#Define maps of interest to extract into lists
maps_of_interest <- parallel_300[,muscle_names()]
colnames(maps_of_interest) <- reference(muscle_names())


test_that("hand 4 ultraflex", {

filename_4A <- "noiseResponse_ST1BC_2017_12_20_23_30_25_SECONDHAND_3tap_all_good_ultraflexed_ignore_stuff_after_null.txt"
hand4_dec20_ultraflex <- fread_df_from_Resilio(filename_4A)
indices_for_null <-  874067:948970
  untransformed_p <- plot_measured_command_reference_over_time(hand4_dec20_ultraflex[indices_for_null,])
  ggsave(to_output_folder(paste0(signals_prefix,filename_4A ,".pdf")),
   untransformed_p, width=90, height=30, limitsize=FALSE)
  noise_response_wo_null <- munge_JR3_data(hand4_dec20_ultraflex,remove_nonzero_map_creation_ids=FALSE, input_are_voltages=TRUE, indices_for_null=indices_for_null, JR3_to_fingertip_distance=JR3_to_fingertip_distance,JR3_sensor_null=NULL)
  p <- plot_measured_command_reference_over_time(noise_response_wo_null)
  ggsave(to_output_folder(paste0("xray_for_",filename_4A ,".pdf")), p, width=90, height=30, limitsize=FALSE)

  group_indices <- list(lower=2,
                        upper=301)
  expect_true(all(maps_match_across_M0_and_map_groups(noise_response_wo_null, group_indices=group_indices, maps_of_interest=maps_of_interest)))
  tails <- extract_trial_tails_by_map_group_indices(noise_response_wo_null, group_indices, 100)
  input_output_data <- dcrb(lapply(tails,colMeans))
  expect_equal(dim(input_output_data), c(300,37))
  write.csv(input_output_data,to_output_folder('hand4_ultraflex_clean.csv'), row.names=FALSE)
})


test_that("hand 4 flex", {

filename_4B <- "noiseResponse_ST1BC_2017_12_20_23_55_41SECONDHAND_3tap_all_good_45_45_10.txt"
hand4_dec20_flex <- fread_df_from_Resilio(filename_4B)
indices_for_null <-  783254:nrow(hand4_dec20_flex)
  untransformed_p <- plot_measured_command_reference_over_time(hand4_dec20_flex[indices_for_null,])
  ggsave(to_output_folder(paste0(signals_prefix,filename_4B ,".pdf")),
   untransformed_p, width=90, height=30, limitsize=FALSE)
  noise_response_wo_null <- munge_JR3_data(hand4_dec20_flex,remove_nonzero_map_creation_ids=FALSE, input_are_voltages=TRUE, indices_for_null=indices_for_null, JR3_to_fingertip_distance=JR3_to_fingertip_distance,JR3_sensor_null=NULL)
  p <- plot_measured_command_reference_over_time(noise_response_wo_null)
  ggsave(to_output_folder(paste0("xray_for_",filename_4B ,".pdf")), p, width=90, height=30, limitsize=FALSE)

  group_indices <- list(lower=2,
                        upper=301)
  expect_true(all(maps_match_across_M0_and_map_groups(noise_response_wo_null, group_indices=group_indices, maps_of_interest=maps_of_interest)))
  tails <- extract_trial_tails_by_map_group_indices(noise_response_wo_null, group_indices, 100)
  input_output_data <- dcrb(lapply(tails,colMeans))
  expect_equal(dim(input_output_data), c(300,37))
  write.csv(input_output_data,to_output_folder('hand4_flex_clean.csv'), row.names=FALSE)
})

quick_fy_plot <- function(data,idxs){
  ggplot(data[idxs,]) + geom_line(aes(time, JR3_FY))
}

test_that("hand 4 extend", {

filename_4C <- "noiseResponse_ST1BC_2017_12_21_01_37_3SECONDHAND_3tap_allgood_moreextended_914_extensormech_posturedep.txt"
hand4_dec20_extend <- fread_df_from_Resilio(filename_4C)
indices_for_null <-  780000:nrow(hand4_dec20_extend)
quick_fy_plot(hand4_dec20_extend, indices_for_null)
  untransformed_p <- plot_measured_command_reference_over_time(hand4_dec20_extend[indices_for_null,])
  ggsave(to_output_folder(paste0(signals_prefix,filename_4C ,".pdf")),
   untransformed_p, width=90, height=30, limitsize=FALSE)
  noise_response_wo_null <- munge_JR3_data(hand4_dec20_extend,remove_nonzero_map_creation_ids=FALSE, input_are_voltages=TRUE, indices_for_null=indices_for_null, JR3_to_fingertip_distance=JR3_to_fingertip_distance,JR3_sensor_null=NULL)
  p <- plot_measured_command_reference_over_time(noise_response_wo_null)
  ggsave(to_output_folder(paste0("xray_for_",filename_4C ,".pdf")), p, width=90, height=30, limitsize=FALSE)

  group_indices <- list(lower=2,
                        upper=301)
  expect_true(all(maps_match_across_M0_and_map_groups(noise_response_wo_null, group_indices=group_indices, maps_of_interest=maps_of_interest)))
  tails <- extract_trial_tails_by_map_group_indices(noise_response_wo_null, group_indices, 100)
  input_output_data <- dcrb(lapply(tails,colMeans))
  expect_equal(dim(input_output_data), c(300,37))
  write.csv(input_output_data,to_output_folder('hand4_extend_clean.csv'), row.names=FALSE)
})


test_that("hand 4 ultraextend", {

filename_4D <- "noiseResponse_ST1BC_2017_12_21_01_56_57_SECONDHAND_3tap_allgood_full_extension_10_10_10_914_extensormech_posture.txt"
hand4_dec20_ultraextend <- fread_df_from_Resilio(filename_4D)
indices_for_null <-  800000:nrow(hand4_dec20_ultraextend)
quick_fy_plot(hand4_dec20_ultraextend, indices_for_null)
  untransformed_p <- plot_measured_command_reference_over_time(hand4_dec20_ultraextend[indices_for_null,])
  ggsave(to_output_folder(paste0(signals_prefix,filename_4D ,".pdf")),
   untransformed_p, width=90, height=30, limitsize=FALSE)
  noise_response_wo_null <- munge_JR3_data(hand4_dec20_ultraextend,remove_nonzero_map_creation_ids=FALSE, input_are_voltages=TRUE, indices_for_null=indices_for_null, JR3_to_fingertip_distance=JR3_to_fingertip_distance,JR3_sensor_null=NULL)
  p <- plot_measured_command_reference_over_time(noise_response_wo_null)
  ggsave(to_output_folder(paste0("xray_for_",filename_4D ,".pdf")), p, width=90, height=30, limitsize=FALSE)

  group_indices <- list(lower=2,
                        upper=301)
  expect_true(all(maps_match_across_M0_and_map_groups(noise_response_wo_null, group_indices=group_indices, maps_of_interest=maps_of_interest)))
  tails <- extract_trial_tails_by_map_group_indices(noise_response_wo_null, group_indices, 100)
  input_output_data <- dcrb(lapply(tails,colMeans))
  expect_equal(dim(input_output_data), c(300,37))
  write.csv(input_output_data,to_output_folder('hand4_ultraextend_clean.csv'), row.names=FALSE)
})
