hand4_ultraextend_clean <- read.csv(to_output_folder("hand4_ultraextend_clean.csv"))
hand4_extend_clean <- read.csv(to_output_folder("hand4_extend_clean.csv"))
hand4_flex_clean <- read.csv(to_output_folder("hand4_flex_clean.csv"))
hand4_ultraflex_clean <- read.csv(to_output_folder("hand4_ultraflex_clean.csv"))
hand3_ultraextend_clean <- read.csv(to_output_folder("hand3_ultraextend_clean.csv"))
hand3_extend_clean <- read.csv(to_output_folder("hand3_extend_clean.csv"))
hand3_flex_clean <- read.csv(to_output_folder("hand3_flex_clean.csv"))
hand3_ultraflex_clean <- read.csv(to_output_folder("hand3_ultraflex_clean.csv"))

test_that('dimensions all match', {
expect_equal(dim(hand4_ultraextend_clean), c(300,37))
expect_equal(dim(hand4_extend_clean), c(300,37))
expect_equal(dim(hand4_flex_clean), c(300,37))
expect_equal(dim(hand4_ultraflex_clean), c(300,37))
expect_equal(dim(hand3_ultraextend_clean), c(300,37))
expect_equal(dim(hand3_extend_clean), c(300,37))
expect_equal(dim(hand3_flex_clean), c(300,37))
expect_equal(dim(hand3_ultraflex_clean), c(300,37))
})

test_that('we can get A from hand4_ultraextend_clean', {

  
})
