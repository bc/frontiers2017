context('test_posture_dependence_cadaver.r')

load_cell_calibrate <- fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_20_16_50_22_500g_loadcell_calibrate_3tap_beforehi_3tap_before_null.txt")
posts <- fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_20_15_55_45_tendons_to_post_good.txt")
hand3_dec20_ultraflex <- fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_20_19_50_38_PD_Extmech_good_ultraflex_NOTAP.txt")
manual_3tap_for_hand3_ultraflex <- fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_20_20_05_37_manual_3tap_extmech_for_ultraflex.txt")
hand3_dec20_flex <- fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_20_19_20_56_PD_extmech_good_45_45_10.txt")
hand3_dec20_extend <- fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_20_20_16_25_3tap_all_good_extended_ish_posture.txt")
hand3_dec20_ultraextend <- fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_20_20_44_47_3tap_all_good_10_10_10_extended_posture.txt")
hand4_dec20_ultraflex <- fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_20_23_30_25_SECONDHAND_3tap_all_good_ultraflexed_ignore_stuff_after_null.txt")
hand4_dec20_flex <-fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_20_23_55_41SECONDHAND_3tap_all_good_45_45_10.txt")
hand4_dec20_extend <-fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_21_01_37_3SECONDHAND_3tap_allgood_moreextended_914_extensormech_posturedep.txt")
hand4_dec20_ultraextend <-fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_21_01_56_57_SECONDHAND_3tap_allgood_full_extension_10_10_10_914_extensormech_posture.txt")

cat <- read.csv("/Users/briancohn/Resilio\ Sync/data/dec20BC1/dec20_PD_EXTMECH/big_jumbo_set_for_posture_dependence_and_extmech_914_NFORCES.csv")[1:350,]
parallel_300 <- cat[1:300,]
replicate_50 <- cat[301:350,]

test_that("maps are unique in parallel", {
  expect_true(length(parallel_300$M0)==length(unique(parallel_300$M0)))
})

test_that("replicates has correct n_unique", {
  expect_equal(length(unique(replicate_50$M0)),5)
})


initial_pass_multiplier = 300
maps_of_interest <- parallel_300[,muscle_names()]
colnames(maps_of_interest) <- reference(muscle_names())

map_indices <- pblapply(df_to_list_of_rows(maps_of_interest), function(map) {
  get_first_map_index(hand3_dec20_ultraflex[seq(1,3e5,by=initial_pass_multiplier),], map)
})
maps_with_ballpark_indices <- cbind(maps_of_interest, ballpark_index=dcrb(map_indices)*initial_pass_multiplier)

exact_first_indices <- dcrb(pblapply(df_to_list_of_rows(maps_with_ballpark_indices), function(map_and_ballpark_index){
  map <- map_and_ballpark_index[,reference(muscle_names())]
  window_bounds <- c(map_and_ballpark_index$ballpark_index-400,map_and_ballpark_index$ballpark_index+400)
  first_map_index <- get_first_map_index(hand3_dec20_ultraflex[,], map, window_bounds = window_bounds)
}))

end_indices <- dcc(lapply(1:nrow(exact_first_indices), function(index) exact_first_indices[index]+diff(exact_first_indices)[index] - 1))
bounds_per_map <- data.frame(start=exact_first_indices, end=end_indices)
#still missing the last map end index.
last_map_start_index <- tail(bounds_per_map$start,1)
last_map_to_trim <- hand3_dec20_ultraflex[seq(last_map_start_index,last_map_start_index+800),]$reference_M0
end_index_for_final_map <- min(which(last_map_to_trim != head(last_map_to_trim,1))) - 2
bounds_per_map[300,2] <- last_map_start_index + end_index_for_final_map
forceTrials <- lapply(df_to_list_of_rows(bounds_per_map), function(map_bounds) {
  hand3_dec20_ultraflex[seq(map_bounds[1],map_bounds[2])]
})

test_that("view how loadcell values are for constant 500g force", {
  raw_signals <- ggplot(load_cell_calibrate) +
  geom_line(aes(time, measured_M0), col = "red")  +
  geom_line(aes(time, measured_M1+3.75), col = "orange")  +
  geom_line(aes(time, measured_M2), col = "yellow") +
  geom_line(aes(time, measured_M3), col = "green") +
  geom_line(aes(time, measured_M4), col = "cyan") +
  geom_line(aes(time, measured_M5), col = "blue") +
  geom_line(aes(time, measured_M6), col = "purple")
  plot(raw_signals)
})

test_that("force_control_works_for_posts", {
  plot_input_output_signals(posts,col_identifier_function=measured, )
 tension_signals <- plot_measured_command_reference_over_time(posts,downsample_amount=100,include_forces=FALSE, include_torques=FALSE)
ggsave(to_output_folder("PD_xray_for_tensions_against_post_dec20.pdf"), tension_signals, width=90, height=30, limitsize=FALSE)
})

test_that('we can extract only the parallel noise part', {
  length(unique(hand3_dec20_flex$map_creation_id))
  hand3_flex_timeseries_raw <- hand3_dec20_flex[4713:243917,]
ggplot(hand3_dec20_flex[4713:243917,])+ geom_line(aes(time, measured_M0))
write.csv(, "hand3_dec20_flex_parallel.csv")
 parts <- split(hand3_dec20_flex[4713:243917,], hand3_dec20_flex[4713:243917,]$map_creation_id)

})
