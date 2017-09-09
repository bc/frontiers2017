options(error = recover)
main <- function() {

  # make sure you run main.r from the figures directory for saving to work
  # setwd('~/Documents/GitHub/bc/frontiers2017/figures') Make sure you have already
  # installed the dependencies: install.packages(c('ggplot2', 'caTools',
  # 'testthat', 'GGally', 'plotrix','scatterplot3d'))
  source("R/data_description.r")
  source("R/time_series_functions.r")
  source("R/functions_specific_to_frontiers2017_dataset.r")
  source("R/settling_time_analysis.r")

  # Fig DataDescription Only take first part of data to create data description
  # figure. (60e3 = first 60 seconds)
  first_data_chunk <- read.csv(data_location, nrows = 85000, header = TRUE)

  # Save snapshot of just first posture
  save_snapshot_for_first_posture(raw_data_timeseries_df = first_data_chunk)

  message("Computing Data Description Analysis Figure")
  pdf("../output/data_description_analysis.pdf", width = 14.2, height = 10)
  data_description_analysis(first_data_chunk, minimum_tendon_force, maximum_tendon_force,
    indices_of_interest = 2:5)
  dev.off()

  # settling_time_analysis(data_location)
  message("Computing Settling Time Analysis")
  message("Pulling full dataset")
  suppressMessages(library(data.table))
  full_df <- readRDS("~/Resilio Sync/data/realTimeData2017_08_16_13_23_42.rds")


  message("Splitting by force trials")
  # unique() pulls the row names from the first occurrences of the unique rows. RM
  # last row of NA values
  unique_postures <- head(unique(full_df[c("adept_x", "adept_y")]), -1)
  browser()
  list_of_postures <- split(full_df, list(full_df$adept_x, full_df$adept_y), drop = TRUE)
}
main()


sample_settling <- data.frame(settling = runif(100, -20, 20), initial_tension = runif(100,
  3, 20), final_tension = runif(100, 3, 20))

##' @param settling data frame with columns: settling, initial_tension, final_tension
##' @return 0 just makes plot of settling~delta_tension
##' if length of a vector V is n, and some q exists s.t. v[q:n] is stable,
##' Then any value 1 < x < Q, where x is stable, implies x:N is also stable.
##' bounds = known stability bounds
##' @export
##' @importFrom WVPlots ScatterHistC
tension_settling_scatter <- function(settling_df = sample_settling) {
  settling_df$delta_tension <- delta_tension(settling_df)
  WVPlots::ScatterHist(settling_df, "delta_tension", "settling", smoothmethod="lm",
                     title="settling~delta_tension", annot_size = 1)
}

##' @param settling data frame with columns: settling, initial_tension, final_tension
##' @return numric vector of signed differences between prior and initial tensions
delta_tension <- function(settling) {
  return(settling$final_tension - settling$initial_tension)
}

tension_settling_scatter()
