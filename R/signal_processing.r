#via http://www.gastonsanchez.com/visually-enforced/how-to/2014/01/15/Center-data-in-R/
##' Center_scale to mean without adjusting variance
##' @param '
center_scale <- function(x) {
    scale(x, scale = FALSE)
}
