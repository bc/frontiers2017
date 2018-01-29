##' returns the input_output_ data samples, labeled for hand3, for each of 4 postures from ultraflexed to ultraextended
##' value computed from last 100ms of force trial time series, when the input signals were stabilized.
hand3_hand4_clean_samples <- function(){

#load and annotate data by hand # and posture
hand4_ultraextend <- read.csv(to_output_folder("hand4_ultraextend_clean_static_response_from_tail_100ms_mean.csv"))
hand4_extend <- read.csv(to_output_folder("hand4_extend_clean_static_response_from_tail_100ms_mean.csv"))
hand4_flex <- read.csv(to_output_folder("hand4_flex_clean_static_response_from_tail_100ms_mean.csv"))
hand4_ultraflex <- read.csv(to_output_folder("hand4_ultraflex_clean_static_response_from_tail_100ms_mean.csv"))
hand3_ultraextend <- read.csv(to_output_folder("hand3_ultraextend_clean_static_response_from_tail_100ms_mean.csv"))
hand3_extend <- read.csv(to_output_folder("hand3_extend_clean_static_response_from_tail_100ms_mean.csv"))
hand3_flex <- read.csv(to_output_folder("hand3_flex_clean_static_response_from_tail_100ms_mean.csv"))
hand3_ultraflex <- read.csv(to_output_folder("hand3_ultraflex_clean_static_response_from_tail_100ms_mean.csv"))

attr(hand4_ultraextend, "hand_number")   <- 4
attr(hand4_extend, "hand_number")        <- 4
attr(hand4_flex, "hand_number")          <- 4
attr(hand4_ultraflex, "hand_number")     <- 4
attr(hand3_ultraextend, "hand_number")   <- 3
attr(hand3_flex, "hand_number")          <- 3
attr(hand3_extend, "hand_number")        <- 3
attr(hand3_ultraflex, "hand_number")     <- 3

attr(hand4_ultraextend, "posture")       <- "ultraextend"
attr(hand4_extend, "posture")            <- "extend"
attr(hand4_flex, "posture")              <- "flex"
attr(hand4_ultraflex, "posture")         <- "ultraflex"
attr(hand3_ultraextend, "posture")       <- "ultraextend"
attr(hand3_extend, "posture")            <- "extend"
attr(hand3_flex, "posture")              <- "flex"
attr(hand3_ultraflex, "posture")         <- "ultraflex"

samples <- list(hand3_ultraextend=hand3_ultraextend,
hand3_extend=hand3_extend,
hand3_flex=hand3_flex,
hand3_ultraflex=hand3_ultraflex,
hand4_ultraextend=hand4_ultraextend,
hand4_extend=hand4_extend,
hand4_flex=hand4_flex,
hand4_ultraflex=hand4_ultraflex)
return(samples)
}
