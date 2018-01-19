
##' get index bounds for each map
##' todo test
##' @param raw_timeseries with columns like reference_M0
##' @param upper_bound_end_of_maps_of_interest the maximum value within raw_timeseries where a map val could be.
##' @param initial_pass_multiplier less than half the length of a normal trial, to make sure you can find all of the maps, e.g. if they only show up for a short time, like 600ms.
##' @return bounds_per_map dataframe where first column is the start index, and second column is the end index. This can be used to split the raw df into it's maps.
find_index_bounds_per_map <- function(raw_timeseries,maps_of_interest,upper_bound_end_of_maps_of_interest, initial_pass_multiplier = 300){
  map_indices <- pblapply(df_to_list_of_rows(maps_of_interest), function(map) {
    get_first_map_index(raw_timeseries[seq(1,upper_bound_end_of_maps_of_interest,by=initial_pass_multiplier),], map)
  })
  maps_with_ballpark_indices <- cbind(maps_of_interest, ballpark_index=dcrb(map_indices)*initial_pass_multiplier)

  exact_first_indices <- dcrb(pblapply(df_to_list_of_rows(maps_with_ballpark_indices), function(map_and_ballpark_index){
    map <- map_and_ballpark_index[,reference(muscle_names())]
    window_bounds <- c(map_and_ballpark_index$ballpark_index-400,map_and_ballpark_index$ballpark_index+400)
    first_map_index <- get_first_map_index(raw_timeseries[,], map, window_bounds = window_bounds)
  }))

  end_indices <- dcc(lapply(1:nrow(exact_first_indices), function(index) exact_first_indices[index]+diff(exact_first_indices)[index] - 1))
  bounds_per_map <- data.frame(start=exact_first_indices, end=end_indices)
  #still missing the last map end index.
  last_map_start_index <- tail(bounds_per_map$start,1)
  last_map_to_trim <- raw_timeseries[seq(last_map_start_index,last_map_start_index+800),]$reference_M0
  end_index_for_final_map <- min(which(last_map_to_trim != head(last_map_to_trim,1))) - 2
  bounds_per_map[300,2] <- last_map_start_index + end_index_for_final_map

  return(bounds_per_map)

}

##' Maps Match
##' todo test
##' @param map1 vector of values
##' @param map2 vector of values, same length as map1
##' @param threshold maximum allowable threshold for any one of the pair-wise comparisons.
##' @return do_they_match logical, true or false. True if all values are only different by a value smaller than the provided
maps_match <- function(map1,map2, threshold){
  if(abs(map1[1]-map2[1]) > 0.01){
    return(FALSE)
  } else {
      return(all(abs(map2-map1) < threshold))
  }
}
##' Get first map index
##' todo test'
##' @param df data frame where there are reference_MX columns
##' @param map_7d 7 dimensional vector where the columns match reference_MX
##' @param threshold maximum allowable threshold. By default 0.0001
##' @param window_bounds lower bound and upper bound guess for the actual map index
get_first_map_index <- function(df, map_7d, threshold = 1e-4, window_bounds = c(0,0)){
  if (window_bounds[1] - window_bounds[2] == 0){
    window_of_interest <- 1:nrow(df)
  } else {
    window_of_interest <- window_bounds[1]:window_bounds[2]
  }
  for (i in window_of_interest) {
    is_match <- maps_match(df[i,reference(muscle_names())], map_7d, threshold)
    if(is_match) return(i)
  }
}
