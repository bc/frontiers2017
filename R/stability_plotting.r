############ Plotting

##' sd_residual_plot_hex
##' @param stability_df dataframe with adept_x and adept_y column, sd, and signed_max_residual
##' @return p plot object for ggplot scatter plot.
sd_residual_plot_hex <- function(stability_df, size = 0.25) {
  my_breaks = c(1, 10, 100, 1000, 10000, 1e+05)
  ggplot(stability_df, aes(sd, signed_max_residual)) + theme_bw() + stat_binhex(bins = 100) +
    scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks) +
    xlab("sd of last 100ms") + ylab("max(residuals of last 100ms)")
}

##' sd_residual_plot_scatter_posture
##' @param list_of_stability_dfs dataframes with adept_x and adept_y column, sd, and signed_max_residual. first is fix_x_sdres
##' @param adept_dimension_that_changes either 'adept_x' or 'adept_y', describing the one that is changing
##' @return p_list plot object for ggplot scatter plot.
sd_residual_plot_scatter_posture <- function(stability_df, adept_dimension_that_changes,
  size = 0.25) {
  my_breaks = c(1, 10, 100, 1000, 10000, 1e+05)
  p <- ggplot(stability_df, aes(sd, signed_max_residual)) + geom_point(size = size) +
    theme_bw() + xlab(paste("sd of last 100ms,", adept_dimension_that_changes)) +
    ylab("max(residuals of last 100ms)") + theme_bw()
  return(p)
}

##' Adept Boxplots to show how posture affects some y
##' the length of the whiskers =  1.5 the IQR
##' @param stability_df dataframe with adept_* and y variable to respond
##' @param adept_dimension_that_changes either 'adept_x' or 'adept_y', describing the one that is changing
##' @param response_variable e.g. 'sd' or 'signed_max_residual'
##' @return p ggplot plot object
adept_boxplots <- function(stability_df, adept_dimension_that_changes, response_variable) {
  adept_range <- range(stability_df[[adept_dimension_that_changes]])
  min_distance_between_adept_postures <- abs(min(diff(unique(stability_df[[adept_dimension_that_changes]])))) *
    0.9
  p <- ggplot(stability_df, aes_string(y = response_variable, group = adept_dimension_that_changes))
  p <- p + geom_boxplot(aes_string(cut_width(stability_df[[adept_dimension_that_changes]],
    min_distance_between_adept_postures)), outlier.alpha = 0.2, outlier.size = 0.1,
    alpha = 0.7, size = 0.5)
  p <- p + theme_bw() + xlab(paste(adept_dimension_that_changes, "postures from", adept_range[1], "to", adept_range[2]))
  p <- p + theme(panel.background = element_rect(fill = "white", colour = "grey50"))
  p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  return(p)
}

##' Relationship between adept posture and signed max residual in scatter plot form
##' @param stability_df dataframe with adept_x and adept_y column, sd, and signed_max_residual
##' @param adept_dimension_that_changes either 'adept_x' or 'adept_y', describing the one that is changing (the one that's not fixed)
##' @return p ggplot object
signed_max_residual_vs_posture <- function(stability_df, adept_dimension_that_changes) {
  p <- ggplot(stability_df, aes_string(adept_dimension_that_changes, "signed_max_residual"))
  p <- p + geom_point(size = 0.1) + theme_bw()
  return(p)
}

##' Produce stability plots in list
##' @param stability_df dataframe with adept_x and adept_y column, sd, and signed_max_residual
##' @param adept_dimension_that_changes either 'adept_x' or 'adept_y', describing the one that is changing (the one that's not fixed)
##' @return p_list list of plot objects
produce_stability_plots <- function(stability_df, adept_dimension_that_changes) {
  p_sd <- adept_boxplots(stability_df, adept_dimension_that_changes, "sd")
  p_max_residual <- signed_max_residual_vs_posture(stability_df, adept_dimension_that_changes)
  p_sdres_adept <- sd_residual_plot_scatter_posture(stability_df, adept_dimension_that_changes)
  p_sd_residual <- sd_residual_plot_hex(stability_df)
  return(list(p_sd, p_max_residual, p_sdres_adept, p_sd_residual))
}
