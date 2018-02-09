load_dynamic_matrix_csv <- function(filename="hand3_ultraflex_clean_timeseries_Meas_fresp.csv"){
   dynamic_source_df <- read.csv(paste0('~/Documents/GitHub/bc/frontiers2017/dynamicAnalysisResults/', filename))
   return(dynamic_source_df)
}
