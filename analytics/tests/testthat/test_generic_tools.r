  source("../../R/generic_tools.r")
require(testthat)
context("Generic Tools")
sample_df_numbers <- data.frame(first = c(1, 5, 4, 2, 5, 2), second = 1:6, row.names = c(5,
  2, 4, 3, 888, 827))
sample_df_letters <- data.frame(first = c(1, 5, 4, 2, 5, 2), second = 1:6, row.names = LETTERS[1:6])
sample_df_row <- data.frame(first = c(1), second = 1, row.names = LETTERS[1])
sample_df_rows <- data.frame(first = c(1, 2, 3, 4, 5, 6), second = 1:6, row.names = LETTERS[1:6])
shuffled_df_rows <- sample(nrow(sample_df_rows), 6, replace = FALSE, prob = NULL)

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
test_that("shuffle_row_wise works with multiple rows", {
  expect_equal(shuffle_row_wise(sample_df_rows), shuffled_df_rows)
})
test_that("shuffle_row_wise does not replace rows", {
  expect_equal(shuffle_row_wise(sample_df_rows), shuffled_df_rows)
})
test_that("shuffle_row_wise works without replacement", {
  expect_equal(shuffle_row_wise(sample_df_rows), shuffled_df_rows)
})
