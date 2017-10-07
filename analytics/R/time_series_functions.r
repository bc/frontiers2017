require(caTools)
force_rise_time_plots <- function(time_vector, y, ...) {
  points(y, col = "grey", pch = 20, cex = 0.25, ...)
  running_mean_ts <- runmean(y, 50, alg = "exact")
  lines(running_mean_ts, col = "blue")
  return(running_mean_ts)
}
force_column_names <- c("JR3.FX", "JR3.FY", "JR3.FZ", "JR3.MX", "JR3.MY", "JR3.MZ")
plot_force_smoothed_curves <- function(time_series_of_forces, force_dimensions = c("JR3.FX",
  "JR3.FY", "JR3.FZ", "JR3.MX", "JR3.MY", "JR3.MZ"), ...) {
  lapply(force_dimensions, function(x) {
    force_rise_time_plots(time_series_of_forces$time, time_series_of_forces[[x]],
      ylab = x, ...)
  })
}

##' Get forces list
##' @description # [-1] was added to remove the nullification force before we
##' changed to a new posture, and the intra-transition data
##' @param full_df dataframe including the column reference_M0 and row.names
##' @param indices where the posture starts and stops. 2 element vector of integers
##' @param column_to_separate_forces string, by default 'reference_M0'.
##' @return forces a list of time_series objects which contain ~800 observations, representing each force trial.
  get_forces_list <- function(full_df, indices, column_to_separate_forces = 'reference_M0'){
    posture_indices <- indices[1]:indices[2]
    forces <- split(full_df[posture_indices,], full_df[posture_indices,column_to_separate_forces])[-1]
    return(forces)
  }


plot_tendon_rise_time_curves <- function(time_series_of_forces, tendon_of_interest_string_list,
  ...) {
  lapply(tendon_of_interest_string_list, function(x) {
    force_rise_time_plots(time_series_of_forces[["time"]], time_series_of_forces[[measured(x)]],
      ylab = x, ...)
    # show the force that was aimed for
    lines(time_series_of_forces[[reference(x)]])
  })
}
# Compose measured_M* string
##' These functions are useful because they prepend measured, reference, or command
##' in front of an input tendon of interest (e.g. M0, M1)
##' @param muscle_string e.g. "M0"
##' @return composed_field_name e.g. "measured_M0"
measured <- function(muscle_string) {
  paste0("measured_", muscle_string)
}
# Compose reference_M* string
##' These functions are useful because they prepend measured, reference, or command
##' in front of an input tendon of interest (e.g. M0, M1)
##' @param muscle_string e.g. "M0"
##' @return composed_field_name e.g. "reference_M0"
reference <- function(muscle_string) {
  paste0("reference_", muscle_string)
}
# Compose command_M* string
##' These functions are useful because they prepend measured, reference, or command
##' in front of an input tendon of interest (e.g. M0, M1)
##' @param muscle_string e.g. "M0"
##' @return composed_field_name e.g. "command_M0"
command <- function(muscle_string) {
  paste0("command_", muscle_string)
}

# Compose encoder angle string
##' These functions are useful because they prepend measured, reference, or command
##' in front of an input tendon of interest (e.g. M0, M1)
##' @param muscle_number e.g. 0, or 1
##' @return composed_field_name e.g. "angle_0"
angle <- function(muscle_number) {
  paste0("angle_", as.character(muscle_number))
}

##' Remove dataframe entires where the robot is moving
##' We remove all observations where robot_flag==1
##' @param time_series A dataframe that has a column called robot_flag, where 0 is initialized, 1 is moving, and 2 is ready.
##' @param ts_trimmed time_series without any incidences of robot_flag as 0 or 2.
rm_points_where_adept_robot_is_moving <- function(time_series) {
  no_2 <- time_series[time_series$robot_flag != 1, ] #ready
  no_0_2 <- no_2[time_series$robot_flag != 0, ] #initialized
  return(no_0_2)  #a dataframe with rows as observations.
}

create_vaf_posture_plots <- function(A_matrices_results) {
  vaf_set <- lapply(A_matrices_results, function(x) {
    compute_vaf_for_fxyz(x)
  })
  vaf_set$adept_x <- unique(static_force_df$adept_x)
  vaf_set$adept_y <- unique(static_force_df$adept_y)
  require(ggplot2)
  require(gridExtra)
  p1 <- ggplot(vaf_set, aes(x = adept_x, y = adept_y))
  point_size_range <- c(0, 5)
  alpha_level <- 1
  num_bins <- 20
  # plot vaf_x
  p_vaf_fx <- p1 + geom_point(aes(color = vaf_fx)) + scale_size(range = point_size_range,
    limits = c(0.61, 0.9167))
  # plot vaf_y
  p_vaf_fy <- p1 + geom_point(aes(color = vaf_fy)) + scale_size(range = point_size_range,
    limits = c(0.61, 0.9167))
  # plot vaf_z
  p_vaf_fz <- p1 + geom_point(aes(color = vaf_fz)) + scale_size(range = point_size_range,
    limits = c(0.61, 0.9167))

  # histogram of error fx
  m_fx <- ggplot(vaf_set, aes(x = vaf_fx)) + geom_histogram(aes(fill = ..count..),
    bins = num_bins) + coord_flip() + xlim(0.6, 1)
  # histogram of error fy
  m_fy <- ggplot(vaf_set, aes(x = vaf_fy)) + geom_histogram(aes(fill = ..count..),
    bins = num_bins) + coord_flip() + xlim(0.6, 1)
  # histogram of error fz
  m_fz <- ggplot(vaf_set, aes(x = vaf_fz)) + geom_histogram(aes(fill = ..count..),
    bins = num_bins) + coord_flip() + xlim(0.6, 1)

  # show all
  ggsave("vaf_fxyz.pdf", arrangeGrob(p_vaf_fx, p_vaf_fy, p_vaf_fz, m_fx, m_fy,
    m_fz, nrow = 2))
  # show just fx for the asb abstract
  ggsave("vaf_fx.pdf", arrangeGrob(p_vaf_fx, m_fx, nrow = 1))

}

compute_vaf_for_fxyz <- function(A_matrix_result) {
  plot3d(A_matrix_result$AMatrix)
}
# This function calculates the variance accounted for between two vectos. The
# first argument is the measured vector The second argument is the predicted
# vector The output is the VAF, a number between 0 and 1
variance_accounted_for_brian <- function(vectorMeasured, vectorPredicted) {

  vectorResidual <- vectorMeasured - vectorPredicted
  if (var(vectorMeasured) == 0) {
    warning("The variance of the measured vector must not be 0")
    return(0)
  }

  resultVAF <- (1 - var(vectorResidual))/var(vectorMeasured)
  return(resultVAF)
}


# This function calculates the variance accounted for between two vectos. The
# first argument is the measured vector The second argument is the predicted
# vector The output is the VAF, a number between 0 and 1
variance_accounted_for <- function(vectorMeasured, vectorPredicted) {
  vectorResidual <- vectorMeasured[[1]] - vectorPredicted[[1]]
  varianceVectorMeasured <- var(vectorMeasured[[1]])
  if (varianceVectorMeasured == 0) {
    # warning('The variance of the measured vector must not be 0')
    resultVAF <- 0
  } else {
    varianceVectorResidual <- var(vectorResidual)
    resultVAF <- 1 - varianceVectorResidual/varianceVectorMeasured
    if (resultVAF < 0) {
      resultVAF <- 0
    }
    if (resultVAF > 1) {
      resultVAF <- 0
    }
  }
  return(resultVAF)
}


change_indices <- function(ts) {
  # derived from http://stackoverflow.com/questions/
  # 20896242/finding-the-index-of-first-changes-in-the-elements-of-a-vector-in-r
  return(1 + which(diff(ts) != 0))
}

grab_section <- function(start_and_lengths, ts) {
  start <- start_and_lengths[1]
  length <- start_and_lengths[2]
  desired_indices <- seq(start, start + length - 1)
  cut_df <- ts[desired_indices, ]
  return(cut_df)
}

split_dataframe_by_indices <- function(ts, indices_vector) {
  start_and_lengths <- cbind(indices_vector[-1], diff(indices_vector))
  list_of_ts <- apply(start_and_lengths, 1, function(x) {
    grab_section(x, ts)
  })
  return(list_of_ts)
}

# make sure col is named adept_x
split_by_position <- function(vector_of_positions, time_series_dataframe) {
  data_split <- lapply(unique(vector_of_positions), function(x_position) {
    return(time_series_dataframe[time_series_dataframe$adept_x == x_position,
      ])
  })
  # Remove the first datapoint, as it's before the data is collected.
  return(data_split[2:length(data_split)])
}

##' Dataframe to list of rows
##' @description derived from https://stackoverflow.com/questions/3492379/data-frame-rows-to-a-list
##' @param df Data frame
##' @return df_list a list of elements, each of which is a representative row from the original df
df_to_list_of_rows <- function(df){
  df_list <- setNames(split(df, seq(nrow(df))), rownames(df))
  return(df_list)
}

# not in function pulled via
# https://stackoverflow.com/questions/5831794/opposite-of-in
`%not in%` <- function(x, table) is.na(match(x, table, nomatch = NA_integer_))

# make sure col is named reference_M0 robotflag int 0 means initialized, 1 means
# moving, 2 means ready motorflag int 0 means initialized, 1 means
# it is applying the force,2 means ready.

split_by_reference_force <- function(time_series_dataframe) {
  forces <- unique(time_series_dataframe$reference_M0)
  forces <- forces[-2]  #remove the nullification force before we changed to a new posture, and the intra-transition data
  list_of_ts_for_diff_forces <- lapply(forces, function(force_m0) {
    time_series_dataframe[time_series_dataframe$reference_M0 == force_m0, ]
  })
  return(list_of_ts_for_diff_forces)
}
# the moment is in units of newton-meters, so we have to divide by meters.
# length is in meters
torque_to_force <- function(torque, length, angle) {
  return(torque/(length * sin(angle)))
}
calibrate_forces <- function(time_series, length, angle) {
  zero_vector <- rep(0, length(time_series[, 1]))
  time_series$JR3.FX <- time_series$JR3.FX + torque_to_force(time_series$JR3.MX,
    length, angle)
  time_series$JR3.FY <- time_series$JR3.FY + torque_to_force(time_series$JR3.MY,
    length, angle)
  time_series$JR3.FZ <- time_series$JR3.FZ + torque_to_force(time_series$JR3.MZ,
    length, angle)
  time_series$JR3.MX <- zero_vector
  time_series$JR3.MY <- zero_vector
  time_series$JR3.MZ <- zero_vector
  return(time_series)
}

##' Convert postures_grouped_by_line to dataframe of start and end indices, with posture XY
##' One element per experiment (where an experiment is a specific set of
##' postures. In the case of Frontiers2017 there are two elements, one for
##' the Y line and one for the X line.
##' @param postures_grouped_by_line The result from the fn postures_grouped_by_line
##' @param unique_postures adept_x, adept_y dataframe with index row names
##' @return idx_df A list of dataframes with the adept_x, adept_y, start index, end index.
postures_to_idx_dfs <- function(postures_grouped_by_line, unique_postures) {
  line_posture_start_indices <- lapply(postures_grouped_by_line, function(line) as.numeric(rownames(line)))
  idxs <- add_adept_xy_to_indices(lapply(line_posture_start_indices, posture_indices_df),
    unique_postures)
  idxs_clean <- clean_up_posture_indices(idxs)
  return(idxs_clean)
}


# This function estimates the A matrix from the measured tendon force and
# endpoint forces and torques
find_A_matrix <- function(data) {
  # The regressor matrix is concatenation of tendon forces
  time <- data[[1]]
  numForceChanges <- length(time)
  force_col_names <- c("JR3.FX", "JR3.FY", "JR3.FZ", "JR3.MX", "JR3.MY", "JR3.MZ")
  muscle_col_names <- c("measured_M0", "measured_M1", "measured_M2", "measured_M3",
    "measured_M4", "measured_M5", "measured_M6")
  raw_regressor <- as.matrix(data[muscle_col_names])
  regressor <- rm_mean_for_multiple_columns(raw_regressor)
  raw_endpointForceObservation <- data[force_col_names]
  endpointForceObservation <- rm_mean_for_multiple_columns(raw_endpointForceObservation)
  AMatrix <- matrix(solve(qr(regressor, LAPACK = TRUE), endpointForceObservation),
    7, 6)
  endpointForcePrediction <- data.frame(regressor %*% AMatrix)
  colnames(endpointForcePrediction) <- force_col_names
  colnames(AMatrix) <- force_col_names
  rownames(AMatrix) <- muscle_col_names
  return(list(AMatrix = AMatrix, endpointForceObservation = endpointForceObservation,
    endpointForcePrediction = endpointForcePrediction))
}

rm_mean_for_multiple_columns <- function(df) {
  col_means <- apply(df, 2, mean)
  for (i in 1:length(col_means)) {
    df[, i] <- df[, i] - col_means[[i]]
  }
  return(df)
}

visualize_A_matrix_performance <- function(A_matrix_results) {
  force_col_names <- c("JR3.FX", "JR3.FY", "JR3.MX", "JR3.MX", "JR3.MY", "JR3.MZ")
  AMatrix <- A_matrix_results[[1]]
  endpointForceObservation <- A_matrix_results[[2]]
  endpointForcePrediction <- A_matrix_results[[3]]
  # visualization
  par(mfrow = c(1, 3))
  for (dimension_of_interest in 1:3) {
    # plot(time,endpointForcePrediction[,dimension_of_interest],type='l',col='blue',
    # ylim=c(-12,12), ylab=force_col_names[dimension_of_interest])
    # matplot(time,endpointForceObservation[,dimension_of_interest],add=T,type='l',col='red',ylim=c(-12,12))
    hist((endpointForcePrediction - endpointForceObservation)[, dimension_of_interest],
      xlim = c(-1, 1), col = "black", freq = TRUE, xlab = paste("Error in Prediction of",
        force_col_names[dimension_of_interest]), main = "")
  }
  return(summary(endpointForcePrediction - endpointForceObservation))
}

calculate_indices_of_convergence_for_muscles <- function(time_series) {
  muscle_measurement_ts <- list(time_series$measured_M0, time_series$measured_M1,
    time_series$measured_M1, time_series$measured_M2, time_series$measured_M3,
    time_series$measured_M4, time_series$measured_M5, time_series$measured_M6)
  return(lapply(muscle_measurement_ts, get_unidimensional_convergence_index))

}

# @param df dataframe with measured_M0, reference_M0, etc @param tendons vector
# of string name of tendons e.g. c('M0', ...) @param returns the residual_MX
compute_tendon_reference_to_measured_residuals <- function(df, tendon) {
  residual_vals <- as.numeric(df[measured(tendon)]) - as.numeric(df[reference(tendon)])
  residual_str_name <- paste0("residual_", tendon)
  assign(residual_str_name, residual_vals)
  return(get(residual_str_name))
}

get_unidimensional_convergence_index <- function(unidimensional_time_series) {
  ts_length <- length(unidimensional_time_series)
  median_at_end <- median(unidimensional_time_series[ts_length - 100:ts_length])
}

wrench_to_phi <- function(vector_3d) {
  x <- vector_3d[1]
  y <- vector_3d[2]
  z <- vector_3d[3]
  rho <- sqrt(x^2 + y^2 + z^2)
  theta <- atan(y/x)
  phi <- atan(sqrt(x^2 + y^2)/z)
  return(list(rho = rho, theta = theta, phi = phi))
}

discrete_diff <- function(vector){
  final <- c(vector[-1], 0)
  initial <- vector
  diff_vec <- final-initial
  return(head(diff_vec,length(vector)-1))
}

##' Extract the forces from each posture.
##' @param idxs a dataframe with cols initial, final, adept_x, and adept_y
##' @param full_df data timeframe with columns of interest
##' @return list of time series dataframes, for each of the postures provided in idxs.
forces_per_posture <- function(idxs, full_df){
    # 0.36252s/posture with lapply
    forces <- lapply(df_to_list_of_rows(idxs), function(row){
      get_forces_list(full_df, indices = c(row[['initial']], row[['final']]))
    })
    return(forces)
}
