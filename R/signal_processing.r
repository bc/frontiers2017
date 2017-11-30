#via http://www.gastonsanchez.com/visually-enforced/how-to/2014/01/15/Center-data-in-R/
##' Center_scale to mean without adjusting variance
##' @param '
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
