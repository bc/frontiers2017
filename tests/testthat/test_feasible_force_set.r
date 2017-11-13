gradient <- colorRampPalette(c("#a6cee3", "#1f78b4", "#b2df8a", "#fc8d62", "#ffffb3",
  "#bebada"))
input_output_per_posture <- read_rds_to_package_extdata("list_of_input_output_data.rds")

context("feasible force set visualization (empirical)")
test_that("experimentally collected output forces match the output of the H matrix model",
  {
    sample <- input_output_per_posture[1:20]
    num_postures <- length(sample)
    rgl_init(bg = "white")
    list_of_mats <- add_gradient_to_attrs(lapply(sample, force3d_matrix, force_column_names[1:3]),
      gradient(num_postures))

    axes_for_multiple_sets(list_of_mats)
    # Add x, y, and z Axes
    lapply(list_of_mats[c(1,5,7)], function(mat) {
      xyz_points_with_convhull(mat, col = attr(mat, "color"), points = FALSE)
    })

  })
