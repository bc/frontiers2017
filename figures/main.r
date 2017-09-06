#make sure you run main.r from the figures directory for saving to work
#setwd('~/Documents/GitHub/bc/frontiers2017/figures')
source('data_description.r')
source('time_series_functions.r')
source('functions_specific_to_frontiers2017_dataset.r')
source('settling_time_analysis.r')

# Define parameters for analysis
muscle_names = c("M0", "M1", "M2","M3","M4","M5","M6")
maximum_tendon_force = 20
minimum_tendon_force = 3
data_location <- "~/Resilio Sync/data/realTimeData2017_08_16_13_23_42.txt"
  
#Fig DataDescription
# Only take first part of data to create data description figure. (60e3 = first 60 seconds)
first_data_chunk <- read.csv(data_location, nrows=85e3, header=TRUE)


#Save snapshot of just first posture
save_snapshot_for_first_posture(raw_data_timeseries_df=first_data_chunk)

pdf("data_description_analysis.pdf", width=14.0, height=10)
  data_description_analysis(first_data_chunk, minimum_tendon_force, maximum_tendon_force, indices_of_interest=2:5)
dev.off()

settling_time_analysis(data_location)
