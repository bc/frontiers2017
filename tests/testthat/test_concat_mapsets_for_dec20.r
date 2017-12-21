#(mid down mid hi) ordering of derivative nudges.
#testthat gen_quartet("M0", parallel[1,]) works
# TODO test
gen_quartet <- function(muscle_of_interest, map) {
  lower <- map
  null <- map
  upper <- map

  lower[1,muscle_of_interest] <- 0.8*map[1,muscle_of_interest]
  upper[1,muscle_of_interest] <- 1.2*map[1,muscle_of_interest]
  quartet <- dcrb(list(map,lower,upper))
  return(quartet)
}

context("test_concatenate_forces_for_posture_dependence_and_extmech.r")
subsample_indices_for_serial <- dcc(lapply(c(0,100,200,300,400,500,600), function(start_index){
  return(start_index + 1:10)
}))

parallel <- read.csv("/Users/briancohn/Desktop/responses/inputs/no_spaces_noise_lo_0_hi_10_nmaps_500_replicates_1__dec3BC1.csv")[1:300,]
replicates <- read.csv("/Users/briancohn/Desktop/responses/inputs/no_spaces_noise_lo_0_hi_10_nmaps_5_replicates_100_dec3BC1.csv")[1:50,]
serial <- read.csv("/Users/briancohn/Desktop/responses/inputs/no_spaces_noise_muscles_actuated_independently_lo_0_hi_10_nmaps_100_replicates_1__dec3BC1.csv")
serial <- serial[subsample_indices_for_serial,]



parallel_without_flexors <- parallel
parallel_without_flexors$M5 <- rep(0.0, nrow(parallel_without_flexors))
parallel_without_flexors$M6 <- rep(0.0, nrow(parallel_without_flexors))
replicates_without_flexors <- replicates
replicates_without_flexors$M5 <- rep(0.0, nrow(replicates_without_flexors))
replicates_without_flexors$M6 <- rep(0.0, nrow(replicates_without_flexors))

nudge_matrix <- dcrb(lapply(df_to_list_of_rows(parallel[1:4,]), function(map){
  dcrb(lapply(c("M0","M1","M2","M3","M4","M5","M6"), function(muscle_name){gen_quartet(muscle_name,map)}))
}
))

nudge_matrix_without_flexors <- dcrb(lapply(df_to_list_of_rows(parallel[1:4,]), function(map){
 dcrb(lapply(c("M0","M1","M2","M3","M4"), function(muscle_name){gen_quartet(muscle_name,map)}))
}
))
 sections <- list(parallel,
replicates,
serial,
parallel_without_flexors,
replicates_without_flexors,
nudge_matrix,
nudge_matrix_without_flexors)

message(sum(dcc(lapply(sections, nrow))))


big_jumbo_set_for_posture_dependence_and_extmech <- dcrb(sections)
write.csv(big_jumbo_set_for_posture_dependence_and_extmech, to_output_folder("big_jumbo_set_for_posture_dependence_and_extmech_914_NFORCES.csv"), row.names=FALSE)
