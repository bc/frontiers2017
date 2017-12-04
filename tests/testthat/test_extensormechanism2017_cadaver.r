context('test_extensormechanism2017_cadaver.r')


test_that("when we run a small sample of 10 forces, I can see the JR3 null at the beginning and end of the trial", {

##TODO
# noise_response_filename_jr3_null_test <- "noise_response_for_jr3_null_test.txt"
# untransformed_noise_response <- as.data.frame(fread(get_Resilio_filepath(noise_response_filename_jr3_null_test)))
## TBD
# untransformed_p <- plot_measured_command_reference_over_time(untransformed_noise_response[3000:10000,])
## TBD
# ggsave(to_output_folder(paste0("get_null_indices_of_beginning_via_this_plot_of_untransformed_xray_for_",noise_response_filename_jr3_null_test ,".pdf")), untransformed_p, width=90, height=30, limitsize=FALSE)

# noise_response_wo_null <- munge_JR3_data(untransformed_noise_response, input_are_voltages=TRUE, JR3_to_fingertip_distance=0.00956, indices_for_null=3000:10000)
# p <- plot_measured_command_reference_over_time(noise_response_wo_null)
# p <- plot_measured_command_reference_over_time(noise_response_wo_null[1:10000,])
# ggsave(to_output_folder(paste0("xray_for_",noise_response_filename_jr3_null_test ,".pdf")), p, width=90, height=30, limitsize=FALSE)

})


test_that("we can produce a FFS vertex series of points to plug into the hand. use different levels so we can evaluate how linear the scaling is for each vertex direction is.", {
  range_tension <- c(0,10)
  binary_combinations <- custom_binary_combinations(7,range_tension)
  many_binary_combination_levels <- lapply(seq(0,1,by=0.2), function(scaling_factor){
    scaling_factor*binary_combinations
  })
  list_of_binary_rampups <- lapply(1:nrow(binary_combinations), function(row_i){
    dcrb(lapply(many_binary_combination_levels, function(block_of_binary_maps){
      block_of_binary_maps[row_i,]
    }))
  })
  list_of_triangle_forcetraces <- lapply(list_of_binary_rampups, function(rampup){
    reversed_rampup <- rampup[nrow(rampup):1,]
    return(rbind(rampup, reversed_rampup))
  })
  df_triangle_forcetraces <- dcrb(list_of_triangle_forcetraces)
  cbound <- cbind(generate_map_creation_ids(nrow(df_triangle_forcetraces)), as.data.frame(df_triangle_forcetraces))
  colnames(cbound) <- c("map_creation_id", muscles_of_interest)
  binary_triangles_filename <- sprintf("binary_vertices_triangle_rampups_lo_%s_hi_%s_total_FORCES_%s.csv",range_tension[1], range_tension[2], nrow(cbound))
  data <- format(cbound, digits=5,scientific=FALSE)
  write.table(data, to_output_folder(binary_triangles_filename), row.names=FALSE, quote=FALSE, sep=",")
  noise_nospc_filename <- paste0("no_spaces_",binary_triangles_filename)
  remove_spaces_command_string <- sprintf('cat %s | tr -d "[:blank:]" > %s',to_output_folder(binary_triangles_filename),to_output_folder(noise_nospc_filename))
  system(remove_spaces_command_string)

})

context("Stage 0: run tendons against post")
test_that("0A looks good",{})
test_that("0B looks good",{})

context("Stage 1: Flexed posture: run tendons against hand, only extensor muscles")
test_that("1A looks good",{})
test_that("1B looks good",{})
test_that("1C looks good",{})
test_that("1C test sample looks good",{

  extensormechanism_1c_filename_jr3_null_test <- "noiseResponse2017_12_04_03_37_03_1Ctest.txt"
  untransformed_noise_response <- as.data.frame(fread(get_Resilio_filepath(extensormechanism_1c_filename_jr3_null_test)))
  ## TBD
  untransformed_p <- plot_measured_command_reference_over_time(untransformed_noise_response[untransformed_noise_response$time<50 & untransformed_noise_response$time >30,])
  ggsave(to_output_folder(paste0("get_null_indices_of_beginning_via_this_plot_of_untransformed_xray_for_",extensormechanism_1c_filename_jr3_null_test ,".pdf")), untransformed_p, width=90, height=30, limitsize=FALSE)
  JR3_null_to_use_for_stage_1 <- colMeans(raw_uncut_timeseries_data[indices_for_null,])
  # p <- plot_measured_command_reference_over_time(noise_response_wo_null)
  # p <- plot_measured_command_reference_over_time(noise_response_wo_null[1:10000,])
  # ggsave(to_output_folder(paste0("xray_for_",extensormechanism_1c_filename_jr3_null_test ,".pdf")), p, width=90, height=30, limitsize=FALSE)
})
test_that("1D looks good",{
  # noise_response_wo_null <- munge_JR3_data(untransformed_noise_response, input_are_voltages=TRUE, JR3_to_fingertip_distance=0.00956, indices_for_null=3000:10000)
})
test_that("1E looks good",{})
test_that("1F looks good",{})

context("Stage:2 Flexed Posture: run tendons against hand, all muscles, using task for stage 1")
test_that("2A looks good",{})
test_that("2B looks good",{})
test_that("2C looks good",{})
test_that("2D looks good",{})
test_that("2E looks good",{})
test_that("2F looks good",{})

  context("Stage:3 Extended Posture: run tendons against hand, only extensor muscles")
test_that("3A looks good",{})
test_that("3B looks good",{})
test_that("3C looks good",{})
test_that("3D looks good",{})
test_that("3E looks good",{})
test_that("3F looks good",{})

  context("Stage:4 Extended Posture: run tendons against hand, all muscles, using task for stage 3")
test_that("4A looks good",{})
test_that("4B looks good",{})
test_that("4C looks good",{})
test_that("4D looks good",{})
test_that("4E looks good",{})
test_that("4F looks good",{})
