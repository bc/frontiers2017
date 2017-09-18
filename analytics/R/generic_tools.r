##' @title rds_from_package_extdata
##' @param filename string, for the file within the extdata folder of the analytics package.
##' @return object the object yielded from the filepath rds
rds_from_package_extdata <- function(filename){
  readRDS(system.file("extdata", filename, package="analytics"))
}

##' @title first_rowname
##' @param df data frame with rownames
##' @param name string for the first rowname
##' TODO write tests
first_rowname <- function(df) head(rownames(df), 1)

##' @title last_rowname
##' @param df data frame with rownames
##' @param name string for the last rowname
##' TODO write tests
last_rowname <- function(df) tail(rownames(df), 1)
