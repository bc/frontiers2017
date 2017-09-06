require(testthat)
stabilized <- function(vector, desired_val, max_allowable_error){
	residuals <- desired_val - vector
#if there are no points that are outside the allowable error
	return(!sum(residuals > max_allowable_error)>0)
}

sample_vec <- c(1,1,1,1,1,1,4,8,9,4,5,3,2,3,2,3,2,3,3,3,3,3,3,3) 
test_that("All indices to the right are within error bounds", {
	expect_that( stabilized(sample_vec, 3, 1), is_false())
	expect_that( stabilized(sample_vec[12:length(sample_vec)], 3, 1), is_true())
  })
