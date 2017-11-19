context('test_functions_specific_to_frontiers2017.r')
source("../../R/functions_specific_to_frontiers2017_dataset.r")
test_that("postures_grouped_by_line", {
  unique_postures <- data.frame(adept_x = c(-516.314298, -531.478918, -525.80549,
    -525, -525, -525, -525, -525, -525, -525, -525), adept_y = c(68, 68, 68,
    63.360715, 63.522459, 61.802527, 72.122261, 65.948095, 72.264025, 62.633837,
    68.007593), row.names = c(81125159, 81206563, 81288007, 81369528, 81450638,
    81531857, 81613207, 81694520, 81775837, 81857174, 81938463))
  x_fixed_value <- -525
  y_fixed_value <- 68
  line_list <- postures_grouped_by_line(unique_postures, x_fixed_value, y_fixed_value)
  expect_equal(line_list[[1]], unique_postures[4:11, ])
  expect_equal(line_list[[2]], unique_postures[1:3, ])
})
