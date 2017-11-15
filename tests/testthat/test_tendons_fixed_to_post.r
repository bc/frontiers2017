context("test_tendons_fixed_to_post.r")
# df <- as.data.frame(fread("/Users/briancohn/Resilio Sync/data/realTimeData2017_09_24_12_25_56.txt"))
# saveRDS(df, 'realTimeData2017_09_24_12_25_56.rds')
df <- readRDS("~/Resilio Sync/data/realTimeData2017_09_24_12_25_56.rds")
colnames(df) <- hyphens_to_dots(colnames(df))


stability_per_muscle <- function(ft_df, muscles_of_interest,...){
  by_muscle_stable_metrics <- lapply(muscles_of_interest, function(x) force_trial_to_stable_metrics(ft_df, 100,x))
  by_muscle_stable_metrics
  by_muscle_stable_dfs <- lapply(muscles_of_interest, function(x) list_of_forces_to_stabilized_df(list(ft_df), err, df, x))
  print('finstabledf')
}

test_that("actions have no effect upon JR3 Force", {
  names(df) <- hyphens_to_underscores(names(df))
  downsampled_df <- df[seq(1, nrow(df), by = 100),]
  p <- ggplot(downsampled_df, aes(x = time))
  p <- p + geom_line(aes(y = JR3_FX), size = 0.1, color="red")
  p <- p + geom_line(aes(y = JR3_FY), size = 0.1, color="green")
  p <- p + geom_line(aes(y = JR3_FZ), size = 0.1, color="blue")
  # p <- p + geom_point(aes(y = JR3_FY/max(JR3_FY)), size = 0.1)
  p
})

test_that("we can extract the forces", {
  #TODO collect forces with 7 tendons all pulling on fixed point.
})
