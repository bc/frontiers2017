
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
  colnames(cbound) <- c("map_creation_id", muscles_of_interest = muscle_names())
  binary_triangles_filename <- sprintf("binary_vertices_triangle_rampups_lo_%s_hi_%s_total_FORCES_%s.csv",range_tension[1], range_tension[2], nrow(cbound))
  data <- format(cbound, digits=5,scientific=FALSE)
  write.table(data, to_output_folder(binary_triangles_filename), row.names=FALSE, quote=FALSE, sep=",")
  noise_nospc_filename <- paste0("no_spaces_",binary_triangles_filename)
  remove_spaces_command_string <- sprintf('cat %s | tr -d "[:blank:]" > %s',to_output_folder(binary_triangles_filename),to_output_folder(noise_nospc_filename))
  system(remove_spaces_command_string)

})
