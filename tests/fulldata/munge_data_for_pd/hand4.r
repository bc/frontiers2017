context('hand 4 dec 20')
cat <- read.csv("/Users/briancohn/Resilio\ Sync/data/dec20BC1/dec20_PD_EXTMECH/big_jumbo_set_for_posture_dependence_and_extmech_914_NFORCES.csv")[1:350,]
parallel_300 <- cat[1:300,]
replicate_50 <- cat[301:350,]
signals_prefix <- "get_null_indices_via_this_plot_of_untransformed_xray_for_"

#Define dec20 hand3 parameters
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
  expect_equal(nrow(input_output_data), 300)
  expect_equal(ncol(input_output_data), 37)
  write.csv(input_output_data,to_output_folder('hand3_ultraextend_clean.csv'), row.names=FALSE)
})