context("breakfreq")
break_frequency <- function(frequency,magnitude){
  break_f <- magnitude[1] - 3
  frequency_index <- head(which(magnitude <= break_f),1)
  return(frequency[frequency_index])
}

test_that("plot many dynamic vs static A matrices. across hands and posture", {
  differences_list <- pblapply(hand3_hand4_clean_static_samples(), function(hand_at_posture) {
    hand_posture_string <- hand_n_posture_string(hand_at_posture)
    dynamic_source_df <- load_dynamic_matrix_csv(paste0(hand_posture_string, "_clean_timeseries_Meas_fresp.csv"))
    input_idxs <- 0:6
    output_idxs <- 0:5
    lapply(input_idxs, function(i){
      dcrb(lapply(output_idxs, function(o){
        bf <- break_frequency(dynamic_source_df[[freq_colname(i,o)]],dynamic_source_df[[mag_colname(i,o)]])
        return(list(bf=bf, muscle=muscle_names()[i+1], force=dots_to_underscores(force_column_names)[o+1]))
      }))
    })
    res <- list(break_freq = break_freq, posture = attr(hand_at_posture, "posture"), hand_number = attr(hand_at_posture, "hand_number"))
    return(res)
  })
})
