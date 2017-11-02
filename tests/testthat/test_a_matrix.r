training_data <-readRDS('/Users/kian/Downloads/training_data.rds')
train_test <- df_split_into_training_and_testing(training_data,
												 fraction_training=0.8)

training_data <- train_test[[1]]
test_data <- train_test[[2]]

	
A_fit <- find_A_matrix(training_data)


#Inspect Residuals
res <- A_fit$endpointForceObservation - A_fit$endpointForcePrediction


 

test_that("unimplemented",{
	expect_equal(5,5)
	})


## library(devtools)
## load_all()
## run the test  a matrix test commands

# To add a breakpoint, use browser(). put browser() anywehre you want to stop.
# type c to exit the browser
# type n to run the next line