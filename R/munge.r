
##' get index bounds for each map
##' todo test
##' @param raw_timeseries with columns like reference_M0
##' @param upper_bound_end_of_maps_of_interest the maximum value within raw_timeseries where a map val could be.
##' @param initial_pass_multiplier less than half the length of a normal trial, to make sure you can find all of the maps, e.g. if they only show up for a short time, like 600ms.
##' @return bounds_per_map dataframe where first column is the start index, and second column is the end index. This can be used to split the raw df into it's maps.
find_index_bounds_per_map <- function(raw_timeseries, maps_of_interest, upper_bound_end_of_maps_of_interest,
  initial_pass_multiplier = 300) {
  map_indices <- pblapply(df_to_list_of_rows(maps_of_interest), function(map) {
    browser()
    get_first_map_index(raw_timeseries[seq(1, upper_bound_end_of_maps_of_interest,
      by = initial_pass_multiplier), ], map)
  })
  maps_with_ballpark_indices <- cbind(maps_of_interest, ballpark_index = dcrb(map_indices) *
    initial_pass_multiplier)

  exact_first_indices <- dcrb(pblapply(df_to_list_of_rows(maps_with_ballpark_indices),
    function(map_and_ballpark_index) {
      map <- map_and_ballpark_index[, reference(muscle_names())]
      window_bounds <- c(map_and_ballpark_index$ballpark_index - 400, map_and_ballpark_index$ballpark_index +
        400)
      first_map_index <- get_first_map_index(raw_timeseries[, ], map, window_bounds = window_bounds)
    }))

  end_indices <- dcc(lapply(1:nrow(exact_first_indices), function(index) exact_first_indices[index] +
    diff(exact_first_indices)[index] - 1))
  bounds_per_map <- data.frame(start = exact_first_indices, end = end_indices)
  # still missing the last map end index.
  last_map_start_index <- tail(bounds_per_map$start, 1)
  last_map_to_trim <- raw_timeseries[seq(last_map_start_index, last_map_start_index +
    800), ]$reference_M0
  end_index_for_final_map <- min(which(last_map_to_trim != head(last_map_to_trim,
    1))) - 2
  bounds_per_map[300, 2] <- last_map_start_index + end_index_for_final_map

  return(bounds_per_map)

}

##' Maps Match
##' todo test
##' @param map1 vector of values
##' @param map2 vector of values, same length as map1
##' @param threshold maximum allowable threshold for any one of the pair-wise comparisons.
##' @return do_they_match logical, true or false. True if all values are only different by a value smaller than the provided
maps_match <- function(map1, map2, threshold) {
  if (abs(map1[1] - map2[1]) > 0.01) {
    return(FALSE)
  } else {
    return(all(abs(map2 - map1) < threshold))
  }
}
##' Get first map index
##' todo test'
##' @param df data frame where there are reference_MX columns
##' @param map_7d 7 dimensional vector where the columns match reference_MX
##' @param threshold maximum allowable threshold. By default 0.0001
##' @param window_bounds lower bound and upper bound guess for the actual map index
get_first_map_index <- function(df, map_7d, threshold = 1e-04, window_bounds = c(0,
  0)) {
  if (window_bounds[1] - window_bounds[2] == 0) {
    window_of_interest <- 1:nrow(df)
  } else {
    window_of_interest <- window_bounds[1]:window_bounds[2]
  }
  for (i in window_of_interest) {
    is_match <- maps_match(df[i, reference(muscle_names())], map_7d, threshold)
    if (is_match)
      return(i)
  }
}


##' print_lower_upper_reference_M0reference_M0_value_at_nth_map_id_group
##' @param df full dataframe of time series, with reference_M0 used as the reference to group maps
##' @param bounds must be a list of lower= x and upper = x. it uses the results from a rleid on the map_creation_id
concat_lower_upper_reference_M0_val_at_id_group <- function(df, bounds) {
    c(reference_M0_value_at_nth_map_id_group(df, bounds$lower), reference_M0_value_at_nth_map_id_group(df,
    bounds$upper))
}

##' maps match across m0 and map groups
##' useful for identifying whether you set up your grouping functions correctly. Sometimes the 2nd through 301st group are actually your desired maps.
##' @param df full timeseries df with m0 column for reference
##' @param group_indices list with lower, and upper value. numeric
##' @param maps_of_interest df with reference_M0, etc.
maps_match_across_M0_and_map_groups <- function(df, group_indices, maps_of_interest,
  threshold = 1e-05) {
  abs(concat_lower_upper_reference_M0_val_at_id_group(df, group_indices) - head_tail_reference_M0(maps_of_interest)) <
    threshold
}


##' head_tail_reference_M0
##' @param maps_of_interest df of refernce_MX and map_creation_id that were input into the system.
##' @param identifies the reference_M0 value for the first and last map of interest.
##' @return tuple_vec vector of two elements, each the numeric value of M0 reference force.
head_tail_reference_M0 <- function(maps_of_interest){
  c(head(maps_of_interest,1)$reference_M0, tail(maps_of_interest,1)$reference_M0)
}

##' reference_M0_value_at_nth_map_id_group
##' @param full_df dataframe that has a reference_M0 column with force values. must also have map_creation_id
##' @param n the group number to check
##' @return the rows where the last value of M0 in the group matches the unique desired one in the map_creaitn_id
reference_M0_value_at_nth_map_id_group <- function(full_df, n){
  indices_to_extract <- tail(which(rleid(full_df$map_creation_id)==n),1)
  full_df[indices_to_extract,]$reference_M0
}

##' extract_trial_tails_by_map_group_indices
##' for the lower and uper indices, extract the map of interest. This way, we
##' do not search the database longer than in necessary.
##' @param full_df including map_creation_id, and reference_M0, etc.
##' @param group_indices list of two elements, lower and upper, integer values for the group index.
##' @param last_n_milliseconds tail number of indices to extract to create colMeans, as fed to input_output_data.
##' @return tails list of dataframe elements, each of which is the stabilized section of the force trial.
extract_trial_tails_by_map_group_indices <- function(full_df, group_indices, last_n_milliseconds){
  tails <- pblapply(group_indices$lower:group_indices$upper, function(map_group_index) {
    return(tail(full_df[rleid(full_df$map_creation_id)==map_group_index,],last_n_milliseconds))
  })
  return(tails)
}