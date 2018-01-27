##' Center_scale to mean without adjusting variance
##' @param x the vector that will be mean-centered
##' @return y mean centered x. not scaled.
center_scale <- function(x) {
    scale(x, scale = FALSE)
}


##' simple low pass filter RC filter
##' First order low pass filter. Sourced from http://biostatmatt.com/R/ecg.R
##' @param y - vector to filter
##' @param t - time interval between measurements (s)
##' @param f - low pass frequency (Hz)
lpf <- function( y, t, f ) {
  rc <- 1 / ( 2 * pi * f )
  a  <- t / ( t + rc )
  n  <- length( y )
  yf <- y
  for( i in 2:length(y) ) {
    yf[i] <- a * y[i] + (1-a) * yf[i-1]
  }
  return( yf )
}

##' Get generators from specific timepoints
##' TODO test with data from "noiseResponse2017_11_28_18_15_09.txt", with sampling snapshots at c(13,24,36,50,63,77,91)
##' lets say you pull on some tendons and want to know the current generator at a given time moment.
##' Use this function to query what the stabilized force is. This also does a running mean on the data]'
##' @param time_vector vector of time in seconds, i.e. 0.010, 0.011, ...
##' @param n_samples_for_running_mean passed to runmean. 30 by default
##' @param vector_of_time_snapshots a list of time points to extract the forces from. After they're running-meaned
##' @return force_snapshots data.frame where each row is a force dimension, and each column corresponds to each of the elements in vector_of_time_snapshots.
take_running_mean_snapshots <- function(force_timeseries_df,time_vector, vector_of_time_snapshots, n_samples_for_running_mean=30){
running_mean_ts <- as.data.frame(apply(force_timeseries_df, 2, runmean, n_samples_for_running_mean, alg = "exact"))
running_mean_ts <- cbind(time=time_vector,running_mean_ts)
force_snapshots <- sapply(vector_of_time_snapshots, function(index){
  head(running_mean_ts[abs(running_mean_ts$time-index) < 1e-2,],1)
})[dots_to_underscores(force_column_names),]
return(force_snapshots)
}
