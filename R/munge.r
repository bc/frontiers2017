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
