% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_series_functions.r
\name{get_forces_list}
\alias{get_forces_list}
\title{Get forces list}
\usage{
get_forces_list(full_df, indices, column_to_separate_forces = "reference_M0")
}
\arguments{
\item{full_df}{dataframe including the column reference_M0 and row.names}

\item{indices}{where the posture starts and stops. 2 element vector of integers}

\item{column_to_separate_forces}{string, by default 'reference_M0'.}
}
\value{
forces a list of time_series objects which contain ~800 observations, representing each force trial.
}
\description{
# [-1] was added to remove the nullification force before we
changed to a new posture, and the intra-transition data
}
