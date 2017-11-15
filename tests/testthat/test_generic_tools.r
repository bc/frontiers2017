source("../../R/generic_tools.r")
require(testthat)
context("Generic Tools")
sample_df_numbers <- data.frame(first = c(1, 5, 4, 2, 5, 2), second = 1:6, row.names = c(5,
  2, 4, 3, 888, 827))
sample_df_letters <- data.frame(first = c(1, 5, 4, 2, 5, 2), second = 1:6, row.names = LETTERS[1:6])
sample_df_row <- data.frame(first = c(1), second = 1, row.names = LETTERS[1])
sample_df_rows <- data.frame(first = c(1, 2, 3, 4, 5, 6), second = 1:6, row.names = LETTERS[1:6])
shuffled_df_rows <- data.frame(first = c(2, 6, 3, 4, 1, 5), second = c(2, 6, 3, 4, 1, 5), row.names = c("B", "F", "C", "D", "A", "E"))
shuffled_unreplaced_rows <- data.frame(first = c(6, 4, 3, 1, 5, 2), second = c(6, 4, 3, 1, 5, 2), row.names = c("F", "D", "C", "A", "E", "B"))


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

context("Resilio filepath composition")
test_that("get resilio filepath creates correct path", {
  filepath <- get_Resilio_filepath('noPostureNeutralForceTrials2017_11_12_14_53_25.txt')
  expect_true(file.exists(filepath))
})
context("Hyphen underscore string operations")
test_that('hyphens_to_underscores', {
  expect_equal(1,1)
  expect_false(1==9)
  expect_equal(hyphens_to_underscores("string1-five"), "string1_five")
  expect_equal(hyphens_to_underscores(1-1), "0")
})

context("Hyphen dots string operations")
test_that('hyphens_to_dots', {
  expect_equal(1-1,0)
  expect_false(1-1==2)
  expect_equal(hyphens_to_dots("-string1-five-"), ".string1.five.")
  expect_equal(hyphens_to_dots("1-1"), "1.1")
})

context("concatenation")
test_that('dcc', {
  A = c(1.5, 2.5, 3)
  B = c(4, 5)
  C = "dog"
  ABC <- list(A,B,C)
  D = c(1.5, 2.5, 3, 4, 5, "dog")
   expect_equal(dcc(ABC), D)
   expect_equal(do.call('c', lapply(1:5, function(x) x)), dcc(lapply(1:5, function(x) x)))
})
