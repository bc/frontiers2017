% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear_fit.r
\name{posture_dependency_plot}
\alias{posture_dependency_plot}
\title{posture dependency bi plot
just makes plot of settling~delta_tension
TODO Create test'}
\usage{
posture_dependency_plot(posture_dependency, independent_variable_name,
  response_variable_name)
}
\arguments{
\item{posture_dependency}{data frame with columns: adepx_x, adept_y, vaf}

\item{independent_variable_name}{string of the independent variable i.e. 'adept_x'}

\item{response_variable_name}{string of the response variable i.e. 'vafs'}
}
\value{
ggplot object of the resultant plot
}
\description{
posture dependency bi plot
just makes plot of settling~delta_tension
TODO Create test'
}
