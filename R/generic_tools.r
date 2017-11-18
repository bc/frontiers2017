##' hyphens_to_underscores
##' @param str string or list of string to do replacement upon
##' @return str_prime string with underscores
hyphens_to_underscores <- function(str){
  gsub("-", "_", str)
}

##' hyphens_to_dots
##' @param str string or list of strings to do replacement upon
##' @return str_prime string with dot
hyphens_to_dots <- function(str){
  gsub("-", ".", str)
}
##' dots_to_underscores
##' @param str string or list of strings to do replacement upon
##' @return str2 dots replaced with underscores
dots_to_underscores <- function(str){
  gsub(".", "_", str, fixed = TRUE)
}

##' Maximum absolute residual
##' TODO test
##' Of all of the absolute residuals from a desired value, this function returns the maximums
##' This is a good measure of the maximum variance.
##' @param vector vector of numeric values
##' @param desired_value numeric, the desired value that all values of the vector should match closely
##' @return max_abs_diff maximum absolute residual
maximum_absolute_residual <- function(vector, desired_val) max(abs(vector - desired_val))

##' Read RDS from package extdata folder
##' @param filename string, for the file within the extdata folder of the analytics package.
##' @return object the object yielded from the filepath rds
read_rds_from_package_extdata <- function(filename){
  # path <- paste0("~/Documents/GitHub/bc/frontiers2017/inst/extdata/",filename)
  path <- system.file("extdata", filename, package="analytics")
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

##' Get Resilio File Path
##' @param filename string of the object of interest, i.e. 'realTimeData2017_08_16_13_23_42.txt'
##' @param filepath string of the object with the path
get_Resilio_filepath <- function(filename){
  paste0("~/Resilio Sync/data/",filename)
}

##' Do call concatenate
##' when output of lapply is a list of elements, where each element is a string or number or integer, this will create a simpified list
##' @param input_list input vector or list that will be concatenated
##' @return output concatenated vector
dcc<- function(input_list) {do.call('c', input_list)}

##' Get Mode from vector
##' if there is no element that is the mode, it returns the first element of the list of equal-occurrence elements.
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

##' Prepend String
##' This is like paste0, but the arguments are reversed. This way you can use it with lapply.
##' @param b string to put in back
##' @param a string to put in front
##' @return a_and_b string concatenated
prepend_string <- function(b, a) {
  paste0(a, b)
}

##' Add a pad values to the left side of the Matrix
##' @param mat matrix that you want to add the pad to
##' @param val_to_repeat int the value you want to repeat
##' @return mat_prime matrix of the updated matrix with the pad
left_pad <- function(mat, val_to_repeat){
  mat_prime <- cbind(rep(1, nrow(mat)),mat)
  return(mat_prime)
}

##' Applies left_pad of 1's
##' @param mat matrix that you want to add the pad to
##' @param 1 int value of the pad to be added
##' @return mat_prime matrix with a left pad of 1's added
left_pad_ones <- function(mat) left_pad(mat, 1)

##' Applies left_pad of 0's
##' @param mat matrix that you want to add the pad to
##' @param 0 int value of the pad to be added
##' @return mat_prime matrix with a left pad of 0's added
left_pad_zeros <- function(mat) left_pad(mat, 0)
