% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/settling_time_analysis.r
\name{discrete_diff}
\alias{discrete_diff}
\title{discrete_diff
Given an array of values, you take the difference between each pair of values, the you return an array of the same size of the original array but -1.}
\usage{
discrete_diff(vector)
}
\arguments{
\item{vector}{numeric vector of values}
}
\value{
differentiated vector of values, with a displacement of 1 index. length 1 less than input.
}
\description{
discrete_diff
Given an array of values, you take the difference between each pair of values, the you return an array of the same size of the original array but -1.
}
