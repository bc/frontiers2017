context('test_har_maps_applied_to_hand.r')
a <- fread(get_Resilio_filepath('noPostureNeutralForceTrials2017_11_14_13_01_05.txt'))
AMatrix <- A_fit$AMatrix
# shows that the output forces are large. that means that the input forces did not evaluate to one value.
P <- dcrb(lapply(df_to_list_of_rows(maps_and_output), function(map_and_id){
  predict_output_force(AMatrix, as.matrix(map_and_id[,c("map_creation_id"):=NULL]))
}))
plot3d(P)
mao <- as.data.frame(a)
forcetrial_dfs <- split(mao[,measured(muscle_names())], mao$map_creation_id)
