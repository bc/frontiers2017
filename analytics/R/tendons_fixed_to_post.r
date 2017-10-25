
##' hyphens_to_underscores
##' @param str string or list of string to do replacement upon
##' @return str_prime string with underscores
hyphens_to_underscores <- function(str){
  gsub("-", "_", str)
}
