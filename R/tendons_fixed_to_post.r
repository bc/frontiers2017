##' add last index number to the postures for the fixed post experiment
##' indices are hard coded from realTimeData2017_09_24_12_25_56.txt
##' @param index_idx_df_without_final_vals dataframe with missing $final column.
##' @return idx_df A list of dataframes with the adept_x, adept_y, start index, end index.
add_fixed_post_specific_end_indices <- function(index_idx_df_without_final_vals){
  line_a_last_index <- 783776
  line_b_last_index <- 1508062
  index_idx_df_without_final_vals$final <- c(line_a_last_index, line_b_last_index)
  return(index_idx_df_without_final_vals)
}
##' Convert posture_list to dataframe of start and end indices, with posture XY, for post
##' One element per experiment (where an experiment is a specific set of
##' postures. In the case of realTimeData2017_09_24_12_25_56.txt there are two elements, one for
##' the Y line and one for the X line.
##' @param posture_list The result from the fn posture_list
##' @param unique_postures adept_x, adept_y dataframe with index row names
##' @return idx_df A list of dataframes with the adept_x, adept_y, start index, end index.
postures_to_idx_dfs_for_post <- function(posture_list, unique_postures) {
  line_posture_start_indices <- lapply(posture_list, function(line) as.numeric(rownames(line)))
  initial_vals <- lapply(line_posture_start_indices, posture_indices_df)
  indices <- add_fixed_post_specific_end_indices(dcrb(initial_vals))
  idx_df <- cbind(indices, unique_postures)
  return(idx_df)
}
