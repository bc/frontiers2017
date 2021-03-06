% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_specific_to_frontiers2017_dataset.r
\name{force_ts_len_is_acceptable}
\alias{force_ts_len_is_acceptable}
\title{Length of the force trial is within range of the acceptable length}
\usage{
force_ts_len_is_acceptable(force_time_series, desired_ms = 800,
  max_delta_acceptable = 50)
}
\arguments{
\item{force_time_series}{Dataframe of observations}

\item{desired_ms}{the ideal length (integer)}

\item{max_delta_acceptable}{integer The maximum aceptable residual from the desired_ms}
}
\value{
is_acceptable logical result
}
\description{
Length of the force trial is within range of the acceptable length
}
