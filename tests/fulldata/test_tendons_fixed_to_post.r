context("test_tendons_fixed_to_post.r")

stability_per_muscle <- function(ft_df, muscles_of_interest,...){
  by_muscle_stable_metrics <- lapply(muscles_of_interest, function(x) force_trial_to_stable_metrics(ft_df, 100,x))
  by_muscle_stable_metrics
  by_muscle_stable_dfs <- lapply(muscles_of_interest, function(x) list_of_forces_to_stabilized_df(list(ft_df), err, df, x))
  print('finstabledf')
}

test_that("maps500_replicates1muscles_7; affixed to wooden post",{
  #Data collected Wed Nov 22
  maps500reps1 <- as.data.frame(fread("~/Resilio\ Sync/data/outputs_nov22/noiseResponse2017_11_22_12_39_21_nmaps_500_replicates_1.txt"))
  muscle_signal_plots <- plot_measured_command_reference_over_time(maps500reps1, include_forces=FALSE)
  ggsave("../../output/xray_for_noiseReponse_maps500reps1_on_posts.pdf", muscle_signal_plots, width=90, height=30, limitsize=FALSE)

})
test_that("maps500_replicates1muscles_7; affixed to wooden post", {
last_n_milliseconds <- 100
  # make sure the JR3 signals respond in some way to the changes.
  maps500reps1_wo_null <- maps500reps1[maps500reps1$map_creation_id != 0, ]
  # Remove pre-experiment and post experiment stuff
  noise_hand_responses_raw <- split_by_map_creation_id(unique(maps500reps1_wo_null$map_creation_id),
    maps500reps1)
  are_correct_length <- dcc(lapply(noise_hand_responses_raw, function(dt) {
    return(nrow(dt) >= 700 && nrow(dt) < 810)
  }))
  noise_hand_responses <- noise_hand_responses_raw[are_correct_length]
  message(sprintf("Out of the %s collected maps, %s had between 700 and 810 samples. Using %s maps.",
    length(noise_hand_responses_raw), length(noise_hand_responses), length(noise_hand_responses)))
  input_output_data <- as.data.frame(df_of_hand_response_input_output(noise_hand_responses, last_n_milliseconds))
  desired_stability_residual <- sapply(df_to_list_of_rows(input_output_data), function(trial){
    trial$reference_M0-trial$measured_M0
  })
  l1_of_maps<- apply(input_output_data[,reference(muscle_names())],1,sum)
  hist(desired_stability_residual)
  print(summary(desired_stability_residual))
  plot(input_output_data$measured_M0, desired_stability_residual)
})
