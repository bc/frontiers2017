context('test_posture_dependence_cadaver.r')

fread_df_from_Resilio <- function(filename){
  fread(get_Resilio_filepath(filename), data.table=FALSE)
}

test_that("we can load the list of map ids that are relevant",
  {
    parallel_300 <- read.csv("/Users/briancohn/Desktop/responses/inputs/no_spaces_noise_lo_0_hi_10_nmaps_500_replicates_1__dec3BC1.csv")[1:300,]
    replicate_50 <- read.csv("/Users/briancohn/Desktop/responses/inputs/no_spaces_noise_lo_0_hi_10_nmaps_5_replicates_100_dec3BC1.csv")[1:50,]
    cat <- read.csv("/Users/briancohn/Resilio\ Sync/data/dec20BC1/dec20_PD_EXTMECH/big_jumbo_set_for_posture_dependence_and_extmech_914_NFORCES.csv")[1:350,]
    expect_equal(length(unique(parallel_300$map_creation_id)), 300)
    expect_equal(length(unique(replicate_50$map_creation_id)), 5)
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

  })
