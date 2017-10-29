##' hyphens_to_underscores
##' TODO test
##' @param str string or list of string to do replacement upon
##' @return str_prime string with underscores
hyphens_to_underscores <- function(str){
  gsub("-", "_", str)
}
##' hyphens_to_dots
##' TODO test
##' @param str string or list of string to do replacement upon
##' @return str_prime string with dot
hyphens_to_dots <- function(str){
  gsub("-", ".", str)
}


##' Read RDS from package extdata folder
##' @param filename string, for the file within the extdata folder of the analytics package.
##' @return object the object yielded from the filepath rds
read_rds_to_package_extdata <- function(filename){
  path <- paste0("~/Documents/GitHub/bc/frontiers2017/inst/extdata/",filename)
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
##' TODO test
##' @param filename string, for the file to place within the extdata folder of the analytics package.
##' @return object the object yielded from the filepath rds
save_rds_to_Resilio <- function(object, filename){
  path <- paste0("~/Resilio Sync/data/",filename)
  # path <- system.file("extdata", filename, package="analytics")
  saveRDS(object, filename)
}
##' Do call concatenate
##" TODO test
##' @param input_list input vector or list that will be concatenated
##' @return output concatenated vector or list.
dcc<- function(input_list) {do.call('c', input_list)}

##' Get Mode from vector
##' if there is no element that is the mode, it returns the first element of the list of equal-occurrence elements.
##' TODO test getmode(c(1,2,3,2,3)) -> 2
##' @param v vector of elements
##' @return mode most common element
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
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
##' @param df dataframe
##' @return df2 dataframe with rows shuffled
shuffle_row_wise <- function(df) df[sample(nrow(df)),]


##' Do Call Rbind on list of dataframes
##' @param list of dataframes
##' @return df row-bound concatenated dataframe
dcrb <- function(list_of_dataframes){
  do.call('rbind', list_of_dataframes)
}
