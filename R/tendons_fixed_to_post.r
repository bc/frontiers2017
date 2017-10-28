
##' hyphens_to_underscores
##' TODO test
##' @param str string or list of string to do replacement upon
##' @return str_prime string with underscores
hyphens_to_underscores <- function(str){
  gsub("-", "_", str)
}


##' Convert postures_grouped_by_line to dataframe of start and end indices, with posture XY, for post
##' One element per experiment (where an experiment is a specific set of
##' postures. In the case of Frontiers2017 there are two elements, one for
##' the Y line and one for the X line.
##' @param postures_grouped_by_line The result from the fn postures_grouped_by_line
##' @param unique_postures adept_x, adept_y dataframe with index row names
##' @return idx_df A list of dataframes with the adept_x, adept_y, start index, end index.
postures_to_idx_dfs_for_post <- function(postures_grouped_by_line, unique_postures) {
  line_posture_start_indices <- lapply(postures_grouped_by_line, function(line) as.numeric(rownames(line)))
  initial_vals <- lapply(line_posture_start_indices, posture_indices_df)
  all_but_final_index <- cbind(dcrb(initial_vals), unique_postures)
  line_a_last_index <- 783776
  line_b_last_index <- 1508062
  all_but_final_index$final <- c(line_a_last_index, line_b_last_index)
  return(all_but_final_index)
}
