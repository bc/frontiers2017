  source("../../R/generic_tools.r")
require(test_that)
context("Generic Tools")
sample_df_numbers <- data.frame(first = c(1, 5, 4, 2, 5, 2), second = 1:6, row.names = c(5,
  2, 4, 3, 888, 827))
sample_df_letters <- data.frame(first = c(1, 5, 4, 2, 5, 2), second = 1:6, row.names = LETTERS[1:6])
sample_df_row <- data.frame(first = c(1), second = 1, row.names = LETTERS[1])
sample_df_rows <- data.frame(first = c(1, 2, 3, 4, 5, 6), second = 1:6, row.names = LETTERS[1:6])
shuffled_df_rows <- data.frame(first = c(2, 6, 3, 4, 1, 5), second = c(2, 6, 3, 4, 1, 5), row.names = c("B", "F", "C", "D", "A", "E"))
shuffled_unreplaced_rows <- data.frame(first = c(6, 4, 3, 1, 5, 2), second = c(6, 4, 3, 1, 5, 2), row.names = c("F", "D", "C", "A", "E", "B"))

sample_list_df <- list(data.frame(a = 1, b = 2), data.frame(a = 1, b = 2))
sample_vector_int <- c(1, 2, 3, 4, 4, 5, 6, 7, 8, 0)
sample_vector_string <- c('A', 'B', 'C', 'D', 'E', 'E')
empty_df <- data.frame(a = NULL, b = NULL)
empty_list_df <-list(data.frame(a = NULL, b = NULL), data.frame(a = NULL, b = NULL))


context("first_rowname")
test_that("first_rowname works with strings", {
  expect_equal(first_rowname(sample_df_letters), "A")
})
test_that("first_rowname works with numbers", {
  expect_equal(first_rowname(sample_df_numbers), "5")
})
context("last_rowname")
test_that("last_rowname works with strings", {
  sample_df <- data.frame(first = c(1, 5, 4, 2, 5, 2), second = 1:6, row.names = LETTERS[1:6])
  expect_equal(last_rowname(sample_df_letters), "F")
})
test_that("last_rowname works with numbers", {
  expect_equal(last_rowname(sample_df_numbers), "827")
})
context("shuffle_row_wise")
test_that("shuffle_row_wise works with one row", {
  expect_equal(shuffle_row_wise(sample_df_row), sample_df_row)
})
set.seed(1) #Sets the seed so that sample produces replicable results
test_that("shuffle_row_wise works with multiple rows", {
  expect_equal(shuffle_row_wise(sample_df_rows), shuffled_df_rows)
})
test_that("shuffle_row_wise does not replace rows", {
  expect_equal(shuffle_row_wise(sample_df_rows), shuffled_unreplaced_rows)
})
test_that("shuffle_row_wise shuffles all rows", {
  expect_equal(nrow(shuffle_row_wise(sample_df_rows)), 6)
})

context("dcrb")
test_that("dcrb works with a list of two different dataframes",{
expect_equal(dcrb(sample_list_df), data.frame(a = c(1,1), b = c(2,2)))
})
#possible warning case , 1 NULL/empty df
test_that("dcrb works with a list of one dataframe and one empty dataframe",{
expect_equal(dcrb(list(empty_df, data.frame(a = 1, b = 2))), data.frame(a = 1, b = 2))
})
# empty elements are NULL

context("all_file_paths")
# test directory
# target a specific directory
# create vector of target elements
# all_file_paths('../') working directory
# expect_equal(all_file_paths('../''), directory_vector)

context("getmode")
test_that("getmode works with integers",{
expect_equal(getmode(sample_vector_int), 4)
})
test_that("getmode works with strings",{
expect_equal(getmode(sample_vector_string), 'E')
})
#possible warning case, 1d vector
test_that("getmode works with 1d vector of int",{
expect_equal(getmode(c(1)), 1)
})
#possible warning case, 1d vector
test_that("getmode works with 1d vector of string",{
expect_equal(getmode(c('A')), 'A')
})
#possible warning case, NULL/empty vector
test_that("getmode works with the NULL vector",{ 
expect_equal(getmode(c()), c())
})
