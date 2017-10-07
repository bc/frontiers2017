##' Read RDS from package extdata folder
##' @param filename string, for the file within the extdata folder of the analytics package.
##' @return object the object yielded from the filepath rds
read_rds_to_package_extdata <- function(filename){
  readRDS(system.file("extdata", filename, package="analytics"))
}

##' @title Save RDS to package extdata folder
##' @param filename string, for the file to place within the extdata folder of the analytics package.
##' @return object the object yielded from the filepath rds
save_rds_to_package_extdata <- function(object, filename){
  saveRDS(object, system.file("extdata", filename, package="analytics"))
}

##' @title first_rowname
##' @param df data frame with rownames
##' @param name string for the first rowname
first_rowname <- function(df) head(rownames(df), 1)

##' @title last_rowname
##' @param df data frame with rownames
##' @param name string for the last rowname
last_rowname <- function(df) tail(rownames(df), 1)

##' Remove columns by a vector of colnames
##' @param timeseries_df dataframe with columns you want to remove
##' @param drops vector of string names of columns to rm
##' @return df_clean dataframe with dropped columns
##' TODO test
rm_cols <- function(timeseries_df, drops) {
  timeseries_df[, !(names(timeseries_df) %in% drops)]
}
