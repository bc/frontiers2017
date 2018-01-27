context('test_posture_dependence_cadaver.r')

hand3_dec20_ultraflex <- fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_20_19_50_38_PD_Extmech_good_ultraflex_NOTAP.txt")
manual_3tap_for_hand3_ultraflex <- fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_20_20_05_37_manual_3tap_extmech_for_ultraflex.txt")

cat <- read.csv(get_Resilio_filepath("dec20BC1/dec20_PD_EXTMECH/big_jumbo_set_for_posture_dependence_and_extmech_914_NFORCES.csv"))[1:350,]
parallel_300 <- cat[1:300,]
replicate_50 <- cat[301:350,]

test_that("maps are unique in parallel", {
  expect_true(length(parallel_300$M0)==length(unique(parallel_300$M0)))
})

test_that("replicates has correct n_unique", {
  expect_equal(length(unique(replicate_50$M0)),5)
})


  load_cell_calibrate <- fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_20_16_50_22_500g_loadcell_calibrate_3tap_beforehi_3tap_before_null.txt")


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
