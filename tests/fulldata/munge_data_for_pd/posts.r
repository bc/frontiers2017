posts <- fread_df_from_Resilio("noiseResponse_ST1BC_2017_12_20_15_55_45_tendons_to_post_good.txt")
#Define maps of interest to extract into lists
cat <- read.csv("/Users/briancohn/Resilio\ Sync/data/dec20BC1/dec20_PD_EXTMECH/big_jumbo_set_for_posture_dependence_and_extmech_914_NFORCES.csv")[1:350,]
parallel_300 <- cat[1:300,]
replicate_50 <- cat[301:350,]

maps_of_interest <- parallel_300[,muscle_names()]
colnames(maps_of_interest) <- reference(muscle_names())

quick_measured_M0_plot <- function(data,idxs){
  ggplot(data[idxs,]) + geom_line(aes(time, measured_M0))
}

test_that("force_control_works_for_posts and we can extract the post data for kian", {
 tension_signals <- plot_measured_command_reference_over_time(posts,downsample_amount=100,include_forces=FALSE, include_torques=FALSE)
 ggsave(to_output_folder("PD_xray_for_tensions_against_post_dec20.pdf"), tension_signals, width=90, height=30, limitsize=FALSE)
 indices_for_null <-  1:nrow(posts)
 quick_measured_M0_plot(posts, indices_for_null)
 group_indices <- list(lower=2,
                       upper=301)
 expect_true(all(maps_match_across_M0_and_map_groups(posts, group_indices=group_indices, maps_of_interest=maps_of_interest)))

 trials <- extract_trials_by_map_group_indices(posts, group_indices)
 first_map_indices <- dcc(lapply(trials[1:10], function(trial){
   head(trial$time,1)
 }))

 posts_snapshot <- ggplot(dcrb(trials[1:10])) + geom_line(aes(time, measured_M0)) + ylim(0,12) + geom_vline(xintercept=first_map_indices, size=0.25, color = "#111111")
ggsave("posts_snapshot.pdf", posts_snapshot)
ggplot(dcrb(lapply(trials, head, 100))) + geom_point(aes(command_M0, measured_M0), size=0.1, alpha=0.5, color = command_M0)

ggplot(dcrb(trials), aes(command_M0, measured_M0)) + geom_point(alpha=0.1, color = 'darkblue')
write.csv(dcrb(trials), to_output_folder("pre_dec20_cadaver_experiment_with_tendons_against_static_posts.csv"), row.names=FALSE)


#Stats
tails_with_residuals<-lapply(lapply(trials, tail, 50),function(tail_data){
  tail_data$M0_residual <- tail_data$measured_M0 - tail_data$reference_M0
  tail_data$M1_residual <- tail_data$measured_M1 - tail_data$reference_M1
  tail_data$M2_residual <- tail_data$measured_M2 - tail_data$reference_M2
  tail_data$M3_residual <- tail_data$measured_M3 - tail_data$reference_M3
  tail_data$M4_residual <- tail_data$measured_M4 - tail_data$reference_M4
  tail_data$M5_residual <- tail_data$measured_M5 - tail_data$reference_M5
  tail_data$M6_residual <- tail_data$measured_M6 - tail_data$reference_M6
  tail_data$reference_l1_sum <- sum(
     head(tail_data$reference_M0 ,1)  + head(tail_data$reference_M1 ,1) + head(tail_data$reference_M2 ,1) + head(tail_data$reference_M3 ,1) + head(tail_data$reference_M4 ,1) + head(tail_data$reference_M5 ,1) + head(tail_data$reference_M6,1))
  return(tail_data)
})

mean_residual_for_each_map <- as.data.frame(dcrb(lapply(tails_with_residuals, colMeans)))
boxplot(mean_residual_for_each_map[,dcc(lapply(muscle_names(), paste0, "_residual"))])

ggplot(mean_residual_for_each_map, aes(reference_l1_sum, M0_residual)) + geom_point()
ggplot(mean_residual_for_each_map, aes(command_M0, M0_residual)) + geom_point()
ggplot(mean_residual_for_each_map, aes(reference_M0, M0_residual)) + geom_point()
ggplot(mean_residual_for_each_map, aes(angle_0, M0_residual)) + geom_point()
ggplot(mean_residual_for_each_map, aes(angle_1, M1_residual)) + geom_point()
ggplot(mean_residual_for_each_map, aes(angle_2, M2_residual)) + geom_point()
ggplot(mean_residual_for_each_map, aes(angle_3, M3_residual)) + geom_point()
ggplot(mean_residual_for_each_map, aes(angle_4, M4_residual)) + geom_point()
ggplot(mean_residual_for_each_map, aes(angle_5, M5_residual)) + geom_point()
ggplot(mean_residual_for_each_map, aes(angle_6, M6_residual)) + geom_point()



})

1
2
3
4
5
6
