context("breakfreq")
break_frequency <- function(frequency,magnitude){
  break_f <- magnitude[1] - 3
  frequency_index <- head(which(magnitude <= break_f),1)
  return(frequency[frequency_index])
}

freq_colnames <- c("freq_i0o0",
"freq_i0o1",
"freq_i0o2",
"freq_i0o3",
"freq_i0o4",
"freq_i0o5",
"freq_i1o0",
"freq_i1o1",
"freq_i1o2",
"freq_i1o3",
"freq_i1o4",
"freq_i1o5",
"freq_i2o0",
"freq_i2o1",
"freq_i2o2",
"freq_i2o3",
"freq_i2o4",
"freq_i2o5",
"freq_i3o0",
"freq_i3o1",
"freq_i3o2",
"freq_i3o3",
"freq_i3o4",
"freq_i3o5",
"freq_i4o0",
"freq_i4o1",
"freq_i4o2",
"freq_i4o3",
"freq_i4o4",
"freq_i4o5",
"freq_i5o0",
"freq_i5o1",
"freq_i5o2",
"freq_i5o3",
"freq_i5o4",
"freq_i5o5",
"freq_i6o0",
"freq_i6o1",
"freq_i6o2",
"freq_i6o3",
"freq_i6o4",
"freq_i6o5")

phase_colnames <- c("phase_i0o0",
"phase_i0o1",
"phase_i0o2",
"phase_i0o3",
"phase_i0o4",
"phase_i0o5",
"phase_i1o0",
"phase_i1o1",
"phase_i1o2",
"phase_i1o3",
"phase_i1o4",
"phase_i1o5",
"phase_i2o0",
"phase_i2o1",
"phase_i2o2",
"phase_i2o3",
"phase_i2o4",
"phase_i2o5",
"phase_i3o0",
"phase_i3o1",
"phase_i3o2",
"phase_i3o3",
"phase_i3o4",
"phase_i3o5",
"phase_i4o0",
"phase_i4o1",
"phase_i4o2",
"phase_i4o3",
"phase_i4o4",
"phase_i4o5",
"phase_i5o0",
"phase_i5o1",
"phase_i5o2",
"phase_i5o3",
"phase_i5o4",
"phase_i5o5",
"phase_i6o0",
"phase_i6o1",
"phase_i6o2",
"phase_i6o3",
"phase_i6o4",
"phase_i6o5")

freq_gain_xys <- function(){
  lapply(input_idxs, function(i){
  lapply(output_idxs, function(o){
    frequencies <- dynamic_source_df[[freq_colname(i,o)]]
    gains <- dynamic_source_df[[mag_colname(i,o)]]
    freq_vs_gain <- data.frame(frequencies=frequencies,gains=gains)
    attr(freq_vs_gain, "force") <- dots_to_underscores(force_column_names)[o+1]
    attr(freq_vs_gain, "muscle") <- muscle_names()[i+1]
    return(list(frequencies=frequencies, gains=gains))})})
  return(freq_gain_xys)
}

magnitude_muscle <- function(str){
  muscle_names()[as.numeric(substr(str, 6,6))+1]
}
magnitude_force <- function(str){
dots_to_underscores(force_column_names)[as.numeric(substr(str, 8,8))+1]
}
extract_frequency_xy <- function(hand_at_posture) {
  hand_posture_string <- hand_n_posture_string(hand_at_posture)
  bootstrap_idxs <- 0:99
  list_of_fresp_bootstrap<-lapply(bootstrap_idxs, function(bootstrap_i){
    dynamic_source_df <- load_dynamic_matrix_csv(paste0(hand_posture_string, "_clean_timeseriesModelMeasFresp",bootstrap_i,".csv"))
    dynamic_source_df$freq <- dynamic_source_df$freq_i0o0
  dynamic_source_df[freq_colnames] <- NULL
  dynamic_source_df[phase_colnames] <- NULL
 freq_tall_df <- melt(dynamic_source_df, id.vars ="freq")
 freq_tall_df$muscle <- magnitude_muscle(freq_tall_df$variable)
 freq_tall_df$force <- magnitude_force(freq_tall_df$variable)
 return(freq_tall_df)
})
  return(list_of_fresp_bootstrap)
}

freq_and_magnitude <- pblapply(hand3_hand4_clean_static_samples(), extract_frequency_xy)
talls <- pblapply(freq_and_magnitude, function(x){
  dcrb(lapply(1:100, function(i){
    df <- x[[i]]
    df$bootstrap_index <- i
    return(df)
  }))
})

one_fresp <- talls[[1]]
one_fresp[one_fresp$force=="JR3_FX,]


tall <-pblapply(freq_and_magnitude, function(x){
  rbind(x)
})