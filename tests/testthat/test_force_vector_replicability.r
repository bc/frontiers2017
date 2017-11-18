context("test_force_vector_replicability.r")
test_that("we can produce viable noise csv with correct number of replicates etc", {
  set.seed(4)
  muscles_of_interest <- muscle_names()
  tension_range <- c(0,20)
  num_replicates <- 3
  num_maps <- 100
  unique_maps <- noise_df(muscles_of_interest, num_maps, tension_range)
  experimental_range <- range(unique_maps[-1])
  expect_true(is_within_range(experimental_range, tension_range))

  replicates <- dcrb(lapply(1:num_replicates, function(x) unique_maps))
  expect_equal(dim(replicates), c(num_replicates*num_maps,length(muscles_of_interest)+1))
  rep_sh <- shuffle_row_wise(replicates)
  noise_filename <- sprintf("Anoise_lo_%s_hi_%s_nmaps_%s_replicates_%s.csv",tension_range[1], tension_range[2], num_maps, num_replicates)
  write.table(format(rep_sh, digits=5), paste0("", noise_filename),
    row.names = FALSE, sep=",",quote=FALSE, dec=".")
})
