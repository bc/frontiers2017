% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_description.r
\name{color_gradient}
\alias{color_gradient}
\title{Create Color Gradient}
\usage{
color_gradient(x, colors = c("blue", "yellow"), colsteps = 100)
}
\arguments{
\item{x}{input numeric data}

\item{colors}{list of string colors that are desired in the gradient. c("blue", "yellow") by default}

\item{colsteps}{number of intervals for the gradient, 100 by default}
}
\value{
color_gradient returns a function that takes an integer argument (the required number of colors) and returns a character vector of colors (see ‘rgb’) interpolating the given sequence (similar to ‘heat.colors’ or ‘terrain.colors’.
}
\description{
Create Color Gradient
}
