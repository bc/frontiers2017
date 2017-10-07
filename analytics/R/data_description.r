library(ggplot2)

plotting_specifics_for_force_control <- function(forces, ylim, ticks){
  plot.new()
  plot.window(xlim=range(ticks),ylim=ylim)
  axis(1, at = ticks)
  axis(2);
  box()
  adept_x_val <- forces[[5]][1, 'adept_x']
  adept_y_val <- forces[[5]][1, 'adept_y']
  title(main="Forces from posture #1", sub=paste("Adept x:",adept_x_val, "Adept y:", adept_y_val),
        xlab="Time (ms)", ylab="Force (N)")

}

plot_muscle_forces_over_time <- function(forces, minimum_tendon_force, maximum_tendon_force, indices_of_interest) {
  ticks = seq(0,length(indices_of_interest)*800, by=400)
  plotting_specifics_for_force_control(forces, ylim=c(minimum_tendon_force, maximum_tendon_force),ticks)
  lapply(list(do.call('rbind',forces[indices_of_interest])),
         function(force_ts) {
           plot_tendon_rise_time_curves(force_ts, tendon_of_interest_string=c("M0", "M1", "M2","M3", "M4", "M5", "M6"), ylim=c(minimum_tendon_force,maximum_tendon_force))
         }
  )
  return(0)
}

plot_JR3_forces_over_time <- function(forces, minimum_tendon_force, maximum_tendon_force, indices_of_interest) {
  #TODO get ts data from data_path instead of economics_long. Color by force + dotted for torques

  sample_of_forces <- forces[indices_of_interest]
  xmax_milliseconds <- length(indices_of_interest)*800
  plotting_specifics_for_force_control(forces, ylim=c(-3.75,1), ticks = seq(0,xmax_milliseconds, by=400))
  smoothed_curves <- lapply(list(do.call('rbind', sample_of_forces)), plot_force_smoothed_curves)
  return(0)
}

wrench_vector_to_labeled_vals <- function(wrench_vector){
  return(paste(
        "FX =",
        wrench_vector[1],
        ", ",
        "FY =",
        wrench_vector[2],
        ", ",
        "FZ =",
        wrench_vector[3],
        ", ",
        "MX =",
        wrench_vector[4],
        ", ",
        "MY =",
        wrench_vector[5],
        ", ",
        "MZ =",
        wrench_vector[6]

                ))
}


compose_dataframe_of_muscle_activation_patterns <- function(forces){
  muscle_activation_patterns <- rbind(
    unique(forces$reference_M0),
    unique(forces$reference_M1),
    unique(forces$reference_M2),
    unique(forces$reference_M3),
    unique(forces$reference_M4),
    unique(forces$reference_M5),
    unique(forces$reference_M6)
  )
  row.names(muscle_activation_patterns) <- muscle_names
  colnames(muscle_activation_patterns) <- LETTERS[1:length(unique(forces$reference_M6))]
  # transpose so the rows are A B and C.
  return(t(muscle_activation_patterns))
}


plot_wrench_text <- function(wrench, x=-2,y=-5, add_SD=FALSE){
  wrench_text <- wrench_vector_to_labeled_vals(wrench)
  if (add_SD){
    wrench_text <- paste("SD :", wrench_text)
  }
  text(x,y, wrench_text, cex=0.20)
}

plot_wrench_SD_text <- function(wrench_SD_vector, x=-2,y=5){
  plot_wrench_text(wrench_SD_vector, x, y, add_SD=TRUE)
}


library(MASS)
generate_parcoord_plot <- function(dataframe_of_observations){
  require(GGally)
  require(ggplot2)
  p_raw <- ggparcoord(dataframe_of_observations, scale='globalminmax', alpha=1, boxplot=FALSE, mapping=ggplot2::aes(colour="midnightblue"))
  p <- p_raw + theme_bw() + theme(
    panel.grid.major.x = element_line(color = "black", size = 0.5),
    panel.grid.major = element_blank(),
    legend.position = "none"
  ) + ylab("Tendon Force (N)") + xlab("Tendon") + ylim(c(0,20))
  return(p)
}














norm_vec <- function(x) sqrt(sum(x^2))

plot_output_wrench_FX_FY <- function(wrench, wrench_sd, xlim, ylim) {
  plot(NA, xlim=xlim, ylim=ylim, main=paste("Norm of w = ", norm_vec(wrench)), xlab=paste("Fx is ", wrench[1]), ylab=paste("Fy is ", wrench[2]), asp=1)
  x = wrench[1]
  y = wrench[2]
  z = wrench[3]
  x_sd = wrench_sd[1]
  y_sd = wrench_sd[2]
  z_sd = wrench_sd[3] #unused as of yet
  segments(0,0,x,y)
  plot_wrench_text(wrench)
  plot_wrench_SD_text(wrench_sd)
  draw_circle_at_end_of_vector(x,y,z)
}

draw_circle_at_end_of_vector <- function(x,y,diameter){
  if (diameter>0) {
    circle_color = "green"
    diameter <- abs(diameter)
  } else {
    circle_color = "black"
  }
  require(plotrix)
  draw.circle(x,y,diameter,border="black",col=circle_color, lty=1,lwd=1)
}


plot_JR3_endpoint_force_vectors <- function(list_of_wrenches, list_of_SD_for_wrenches, xlim=c(-5,5), ylim=c(-5,5), zlim=c(-5,5)) {
  num_wrenches <- length(list_of_wrenches)
  par(mfrow=c(1,num_wrenches))
  lapply(1:num_wrenches, function(x){
    plot_output_wrench_FX_FY(list_of_wrenches[[x]], list_of_SD_for_wrenches[[x]], xlim,ylim)
  })
  par(mfrow=c(1,1))
}

#Get the mean of the last n force values for each of the force signals, for the list elements specified by the indices_of_interest
# forces is a list of ramp&hold time series dataframes
# force_column_names is a vecotr of character strings relating to the columns of forces[[i]] that contain force recordings
list_of_mean_of_last_n_observations <- function(forces, indices_of_interest, n,force_column_names) {
  list_of_tail_wrench_means <- lapply(forces[indices_of_interest], function(x){
    colMeans(
      tail(x[force_column_names],100)
    )
  })
  return(list_of_tail_wrench_means)
}
#Get the SD of the last n force values for each of the force signals, for the list elements specified by the indices_of_interest
# forces is a list of ramp&hold time series dataframes
# force_column_names is a vecotr of character strings relating to the columns of forces[[i]] that contain force recordings
sd_of_last_n_observations <- function(forces,indices_of_interest,n=100, force_column_names) {
  sd_for_last_n_obs <- lapply(forces[indices_of_interest], function(x) {
    apply(
      tail(x[force_column_names],n)
      , 2, sd)
  })
return(sd_for_last_n_obs)
}

plot_porcupine_of_endpoint_wrenches <- function(forces) {
  wrench_observation_df <- do.call('rbind', forces) #only grab the first posture
  force_ranges <- apply(wrench_observation_df,2,range)
  par(mfrow=c(2,1))
  require(scatterplot3d)

  scatterplot3d(wrench_observation_df[,1], wrench_observation_df[,2], wrench_observation_df[,3], pch=16,
                xlim = force_ranges[,1],
                ylim = force_ranges[,2],
                zlim = force_ranges[,3],
                xlab= "Fx", ylab="Fy",zlab="Fz" , highlight.3d=TRUE,
                type="h", main="Recorded output forces")

  scatterplot3d(wrench_observation_df[,4], wrench_observation_df[,5], wrench_observation_df[,6], pch=16,
                xlim = force_ranges[,4],
                ylim = force_ranges[,5],
                zlim = force_ranges[,6],
                xlab= "Mx", ylab="My",zlab="Mz" ,highlight.3d=TRUE,
                type="h", main="Recorded output moments")

  par(mfrow=c(1,1))
  color.gradient <- function(x, colors=c("blue","yellow"), colsteps=100) {
    return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x), max(x), length.out=colsteps)) ] )
  }
  gradient_colors = c("blue","yellow")
  z_colors <- color.gradient(wrench_observation_df[,3], gradient_colors)
  z_range <- range(wrench_observation_df[,3])
  z_range_distance <- z_range[2]-z_range[1]
  z_range_midpoint <- z_range_distance/2 + z_range[1]
  plot(1,type='n', xlim=c(force_ranges[1,1], 0), ylim=c(-3,3), asp=1, xlab="Fx", ylab="Fy", main=paste("n = ", length(forces)))
  origin_points <- rep(0,length(forces))
  segments(origin_points,origin_points, wrench_observation_df[,1], wrench_observation_df[,2],col=z_colors)
  points(wrench_observation_df[,1],wrench_observation_df[,2], xlim=c(force_ranges[1,1], 0), ylim=c(-3,3), col=z_colors, asp=1, xlab="Fx", ylab="Fy", main=paste("n = ", length(forces)))

  # Illustrate the gradient's relationship to the scale
  legend_image <- as.raster(matrix(colorRampPalette(gradient_colors) (100), ncol=1))
  values_to_label_on_legend <- seq(z_range[1],z_range[2],l=6)
  rasterImage(legend_image, -1.5, 1, -1,3)
  text(x=-1, y = seq(1,3,l=length(values_to_label_on_legend)), labels = signif(values_to_label_on_legend,3), adj = c(0, 0.5))
}

data_description_analysis <- function(first_data_chunk, minimum_tendon_force, maximum_tendon_force, indices_of_interest){
  postures <- split_by_position(first_data_chunk$adept_x, first_data_chunk)
  forces <- unlist(lapply(postures, split_by_reference_force), recursive=FALSE)

  #Parcoord - muscle activation patterns
  muscle_activation_patterns <- do.call('rbind', lapply(forces[indices_of_interest], compose_dataframe_of_muscle_activation_patterns))
  row.names(muscle_activation_patterns) <- LETTERS[1:length(indices_of_interest)]
  p <- generate_parcoord_plot(muscle_activation_patterns)
  plot(p)

  #Implemented Tensions
  par(mfrow=c(1,1))
  plot(plot_muscle_forces_over_time(forces, minimum_tendon_force, maximum_tendon_force, indices_of_interest))

  # Resultant Wrenches over time
  par(mfrow=c(1,1))
  plot(plot_JR3_forces_over_time(forces, minimum_tendon_force, maximum_tendon_force, indices_of_interest))

  #Visualization of output Wrenches
  list_of_tail_wrench_mean <- list_of_mean_of_last_n_observations(forces, indices_of_interest, n=100, force_column_names)
  list_of_tail_wrench_SD <- sd_of_last_n_observations(forces,indices_of_interest,n=100, force_column_names)

  list_of_wrenches <- lapply(list_of_tail_wrench_mean, as.numeric)
  list_of_SD_for_wrenches <- lapply(list_of_tail_wrench_SD, as.numeric)

  xlim = range(lapply(list_of_wrenches, function(x){x[1]}))
  ylim = range(lapply(list_of_wrenches, function(x){x[2]}))
  zlim = range(lapply(list_of_wrenches, function(x){x[3]}))
  #don't let the circle get out of the box
  xlim[1] = xlim[1] - max(abs(zlim))
  #either take 0 or the rightmost circle edgepoint.
  xlim[2] = max(c(xlim[2] + max(abs(zlim)), 0))

  #Show 3D endpoint vectors for a couple postures as a sanity check
  plot_JR3_endpoint_force_vectors(list_of_wrenches, list_of_SD_for_wrenches, xlim, ylim, zlim)
  indices_of_correctly_lengthed_forces <- which(lapply(forces, force_ts_len_is_acceptable) == TRUE)

  force_extended_list <- list_of_mean_of_last_n_observations(forces,indices_of_interest=1:length(forces), n=100,force_column_names)
  posture_1_viable_forces <- force_extended_list[indices_of_correctly_lengthed_forces]
  plot_porcupine_of_endpoint_wrenches(posture_1_viable_forces)

  # How long do trials take?
  elapsed_trial_times <- plot_force_trial_elapsed_time_distribution(forces[indices_of_correctly_lengthed_forces])
}
plot_force_trial_elapsed_time_distribution <- function(forces){
  vector_of_times <- do.call('c',lapply(forces, function(x) length(x[,1])))
  hist(vector_of_times, breaks=10, xlab="Time per force trial (ms)", ylab="Number of force trials", main="Force trial length histogram (count)", col='#000000')
  return(vector_of_times)
}
