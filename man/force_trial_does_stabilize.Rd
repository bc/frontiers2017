% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/settling_time_analysis.r
\name{force_trial_does_stabilize}
\alias{force_trial_does_stabilize}
\title{force_trial_does_stabilize
Evaluate whether a force trial has stabilized for a given muscle.
TODO Create Test'}
\usage{
force_trial_does_stabilize(force_trial_df, muscle, err)
}
\arguments{
\item{force_trial_df}{data.frame of numeric values, that includes the reference and measured columns}

\item{muscle}{muscle of interest string, e.g. 'M0'}

\item{err}{numeric the maximum allowable residual for a given value from the desired value.}

\item{desired}{numeric the desired stabilized value for the vector, if the vector is 'stabilized'}
}
\value{
last_value_is_in_range TRUE if last value is in range, false if last value is not in range
}
\description{
force_trial_does_stabilize
Evaluate whether a force trial has stabilized for a given muscle.
TODO Create Test'
}
