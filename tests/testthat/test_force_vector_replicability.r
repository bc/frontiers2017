context("test_force_vector_replicability.r")
test_that("we can produce viable noise csv with correct number of replicates etc", {
  set.seed(4)
  muscles_of_interest <- muscle_names()
  range_tension <- c(0,20)
  num_replicates <- 1
  num_maps <- 500
  unique_maps <- noise_df(muscles_of_interest, num_maps, range_tension)
  experimental_range <- range(unique_maps[-1])
  expect_true(is_within_range(experimental_range, range_tension))
  replicates <- dcrb(lapply(1:num_replicates, function(x) unique_maps))
  expect_equal(dim(replicates), c(num_replicates*num_maps,length(muscles_of_interest)+1))
  rep_sh <- shuffle_row_wise(replicates)
  noise_filename <- sprintf("noise_lo_%s_hi_%s_nmaps_%s_replicates_%s.csv",range_tension[1], range_tension[2], num_maps, num_replicates)
  data <- format(rep_sh, digits=5,scientific=FALSE)
  write.table(data, noise_filename, row.names=FALSE, quote=FALSE, sep=",")
  noise_nospc_filename <- paste0("no_spaces_",noise_filename)
  remove_spaces_command_string <- sprintf('cat %s | tr -d "[:blank:]" > %s',noise_filename,noise_nospc_filename)
  system(remove_spaces_command_string)
})

test_that("we can produce viable noise csv with few maps but MANY replicates", {
  set.seed(4)
  muscles_of_interest <- muscle_names()
  range_tension <- c(0,20)
  num_replicates <- 100
  num_maps <- 5
  unique_maps <- noise_df(muscles_of_interest, num_maps, range_tension)
  experimental_range <- range(unique_maps[-1])
  expect_true(is_within_range(experimental_range, range_tension))
  replicates <- dcrb(lapply(1:num_replicates, function(x) unique_maps))
  expect_equal(dim(replicates), c(num_replicates*num_maps,length(muscles_of_interest)+1))
  rep_sh <- shuffle_row_wise(replicates)
  noise_filename <- sprintf("noise_lo_%s_hi_%s_nmaps_%s_replicates_%s.csv",range_tension[1], range_tension[2], num_maps, num_replicates)
  data <- format(rep_sh, digits=5,scientific=FALSE)
  write.table(data, noise_filename, row.names=FALSE, quote=FALSE, sep=",")
  noise_nospc_filename <- paste0("no_spaces_",noise_filename)
  remove_spaces_command_string <- sprintf('cat %s | tr -d "[:blank:]" > %s',noise_filename,noise_nospc_filename)
  system(remove_spaces_command_string)
})
