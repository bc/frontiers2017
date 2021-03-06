% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_description.r
\name{list_of_mean_of_last_n_observations}
\alias{list_of_mean_of_last_n_observations}
\title{list_of_mean_of_last_n_observations
Get the mean of the last n force values for each of the force signals, for the
list elements specified by the indices_of_interest
forces is a list of ramp&hold time series dataframes
force_column_names is a vector of character strings relating to the columns of forces[[i]] that contain force recordings
TODO Create Test}
\usage{
list_of_mean_of_last_n_observations(forces, indices_of_interest,
  last_n_milliseconds, force_column_names)
}
\arguments{
\item{forces}{list of timeseries_dfs}

\item{indices_of_interest}{the elements within the forces that will be included in the analysis}

\item{last_n_milliseconds}{the number of tail milliseconds from which we should calculate the settled mean}

\item{force_column_names}{string list}
}
\value{
list_of_tail_wrench_means computed means of the last n milliseconds of the force columns
}
\description{
list_of_mean_of_last_n_observations
Get the mean of the last n force values for each of the force signals, for the
list elements specified by the indices_of_interest
forces is a list of ramp&hold time series dataframes
force_column_names is a vector of character strings relating to the columns of forces[[i]] that contain force recordings
TODO Create Test
}
