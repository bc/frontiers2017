% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/settling_time_analysis.r
\name{get_reference_value}
\alias{get_reference_value}
\title{get_reference_value
Gets reference value from specific row of dataframe
TODO test}
\usage{
get_reference_value(index_target, full_df, muscle_of_interest)
}
\arguments{
\item{index_target}{int; the index from which we will extract the muscle of interest's reference force}

\item{full_df}{full dataset from the .rds}

\item{muscle_of_interest}{string, e.g. 'M0'}
}
\value{
full_df numeric reference value from muscle of interest '
}
\description{
get_reference_value
Gets reference value from specific row of dataframe
TODO test
}
