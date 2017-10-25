contest('Test tendons fixed to a nonmoving post')

test_that('actions have no effect upon JR3 Force', {


  df <- fread("/Users/briancohn/Resilio\ Sync/data/realTimeData2017_09_24_12_25_56.txt")
  names(df) <- hyphens_to_underscores(names(df))
  downsampled_df <- df[seq(1,nrow(df), by=100)]
  p <- ggplot(downsampled_df, aes(x = time))
  p <- p + geom_line(aes(y=reference_M0/max(reference_M0), color=measured_M0), size=0.1)
  p <- p + geom_line(aes(y=adept_x), size=0.1)
  p <- p + geom_line(aes(y=adept_y), size=0.1)
  p <- p + geom_point(aes(y=JR3_FY/max(JR3_FY)), size=0.1)
  p
  

})
