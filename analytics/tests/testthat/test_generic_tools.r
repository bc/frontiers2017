  source("../../R/generic_tools.r")
require(testthat)
context("Generic Tools")
sample_df_numbers <- data.frame(first = c(1, 5, 4, 2, 5, 2), second = 1:6, row.names = c(5,
  2, 4, 3, 888, 827))
sample_df_letters <- data.frame(first = c(1, 5, 4, 2, 5, 2), second = 1:6, row.names = LETTERS[1:6])

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
