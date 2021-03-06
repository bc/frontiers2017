% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_series_functions.r
\name{rm_points_where_adept_robot_is_moving}
\alias{rm_points_where_adept_robot_is_moving}
\title{Remove dataframe entires where the robot is moving
We remove all observations where robot_flag==1}
\usage{
rm_points_where_adept_robot_is_moving(time_series)
}
\arguments{
\item{time_series}{A dataframe that has a column called robot_flag, where 0 is initialized, 1 is moving, and 2 is ready.}

\item{ts_trimmed}{time_series without any incidences of robot_flag as 0 or 2.}
}
\description{
Remove dataframe entires where the robot is moving
We remove all observations where robot_flag==1
}
