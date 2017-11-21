context('test_linear_fit.r')
last_n_milliseconds = 100
test_that("we can visualize the output forces in different groups", {
  list_of_input_output_data <- readRDS("list_of_input_output_data.rds")
  firstposture <- list_of_input_output_data[[1]]
  all_fts <- dcrb(list_of_input_output_data)

  x_is_fixed <- dcc(lapply(list_of_input_output_data, function(x) {
    attr(x, "adept_coordinates")[1] == -525
  }))

  adept_x_coords <- dcc(lapply(list_of_input_output_data, function(x) attr(x, "adept_coordinates")[1]))
  adept_y_coords <- dcc(lapply(list_of_input_output_data, function(x) attr(x, "adept_coordinates")[2]))
  along_y_line <- dcrb(list_of_input_output_data[which(x_is_fixed)])
  along_x_line <- dcrb(list_of_input_output_data[which(!x_is_fixed)])

  expect_true(sum(attr(firstposture, "adept_coordinates")) - sum(c(-515.0074, 68)) <
    0.001)

  roygbiv <- c("red", "orange", "yellow", "green", "blue", "violet")
  rgl.open()
  rgl.bg(color = "white")
  plot3d(along_y_line$JR3.FX, along_y_line$JR3.FY, along_y_line$JR3.FZ, sub = "along_y_line of postures",
    aspect = TRUE)
  rgl.open()
  rgl.bg(color = "white")
  plot3d(along_x_line$JR3.FX, along_x_line$JR3.FY, along_x_line$JR3.FZ, sub = "along_x_line of postures",
    aspect = TRUE)
})
