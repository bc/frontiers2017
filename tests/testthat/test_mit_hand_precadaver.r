context('test_mit_hand_precadaver.r')

range_tension <- c(0,20)
force_names_to_predict <- dots_to_underscores(force_column_names)
generators_force_columns_500g <- as.matrix(read_rds_from_package_extdata("generator_force_vectors_for_each_muscle_500g_mit_hand.rds"))
# generators_force_columns_500g variable is in the exact same format that A_fit$AMatrix
binary_combinations <- custom_binary_combinations(ncol(generators_force_columns_500g), range_tension)
matrix_version_of_generators <- matrix(sapply(generators_force_columns_500g, as.numeric), ncol=7)
binary_combination_ffs_points <- matrix_version_of_generators %*% t(binary_combinations)
ffs_binary <- t(binary_combination_ffs_points)

test_that("500g recorded forces give correct FFS for forces", {
  rgl.init()
  plot_ffs_with_vertices(ffs_binary[,1:3], t(matrix_version_of_generators)[,1:3], alpha_transparency=0.25, range_tension=range_tension)
  # input_output_data <- get from random noise experiments
  # points3d(input_output_data[,force_names_to_predict][,1:3], size=1, col="black", alpha=1)
  title3d(main="FFS", xlab="Fx", ylab="Fy", zlab="Fz", col="black")
})

test_that("500g recorded forces give correct FFS for forces", {
  rgl.init()
  plot_ffs_with_vertices(ffs_binary[,4:6], t(matrix_version_of_generators)[,4:6], alpha_transparency=0.25, range_tension=range_tension)
  # input_output_data <- get from random noise experiments
  # points3d(input_output_data[,force_names_to_predict][,1:3], size=1, col="black", alpha=1)
  title3d(main="FFS for Torques", xlab="Mx", ylab="My", zlab="Mz", col="black")
})
