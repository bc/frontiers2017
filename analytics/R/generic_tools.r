##' Read RDS from package extdata folder
##' @param filename string, for the file within the extdata folder of the analytics package.
##' @return object the object yielded from the filepath rds
read_rds_to_package_extdata <- function(filename){
  path <- paste0("~/Documents/GitHub/bc/frontiers2017/analytics/inst/extdata/",filename)
  # path <- system.file("extdata", filename, package="analytics")
  readRDS(path)
}

##' @title Save RDS to package extdata folder
##' @param filename string, for the file to place within the extdata folder of the analytics package.
##' @return object the object yielded from the filepath rds
save_rds_to_package_extdata <- function(object, filename){
  path <- paste0("~/Documents/GitHub/bc/frontiers2017/analytics/inst/extdata/",filename)
  # path <- system.file("extdata", filename, package="analytics")
  saveRDS(object)
}

##' @title Save RDS to resilio sync folder
##' @param filename string, for the file to place within the extdata folder of the analytics package.
##' @return object the object yielded from the filepath rds
save_rds_to_Resilio <- function(object, filename){
  path <- paste0("~/Resilio Sync/data/",filename)
  # path <- system.file("extdata", filename, package="analytics")
  saveRDS(object, filename)
}

##' @title first_rowname
##' @param df data frame with rownames
##' @param name string for the first rowname
first_rowname <- function(df) head(rownames(df), 1)

##' @title last_rowname
##' @param df data frame with rownames
##' @param name string for the last rowname
last_rowname <- function(df) tail(rownames(df), 1)

##' Get full paths of all files within a directory
##' useful when you want to map across items within an entire directory.
##' @param pwd_of_directory the full path to get to the file
##' @return filepaths vector of strings of all filepaths within the provided dir
all_file_paths <- function(pwd_of_directory){
  simplify2array(lapply(dir(pwd_of_directory), prepend_string, pwd_of_directory))
}

##' Row-Wise Shuffle a Dataframe
##' TODO test
##' @param df dataframe
##' @return df2 dataframe with rows shuffled
shuffle_row_wise <- function(df) df[sample(nrow(df)),]
