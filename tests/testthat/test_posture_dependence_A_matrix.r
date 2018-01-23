hand4_ultraextend_clean <- read.csv(to_output_folder("hand4_ultraextend_clean.csv"))
hand4_extend_clean <- read.csv(to_output_folder("hand4_extend_clean.csv"))
hand4_flex_clean <- read.csv(to_output_folder("hand4_flex_clean.csv"))
hand4_ultraflex_clean <- read.csv(to_output_folder("hand4_ultraflex_clean.csv"))
hand3_ultraextend_clean <- read.csv(to_output_folder("hand3_ultraextend_clean.csv"))
hand3_extend_clean <- read.csv(to_output_folder("hand3_extend_clean.csv"))
hand3_flex_clean <- read.csv(to_output_folder("hand3_flex_clean.csv"))
hand3_ultraflex_clean <- read.csv(to_output_folder("hand3_ultraflex_clean.csv"))

muscles_of_interest <- muscle_names()
force_names_to_predict <- dots_to_underscores(force_column_names)


test_that('dimensions all match', {
expect_equal(dim(hand4_ultraextend_clean), c(300,37))
expect_equal(dim(hand4_extend_clean), c(300,37))
expect_equal(dim(hand4_flex_clean), c(300,37))
expect_equal(dim(hand4_ultraflex_clean), c(300,37))
expect_equal(dim(hand3_ultraextend_clean), c(300,37))
expect_equal(dim(hand3_extend_clean), c(300,37))
expect_equal(dim(hand3_flex_clean), c(300,37))
expect_equal(dim(hand3_ultraflex_clean), c(300,37))
})

test_that('we can get A from hand4_ultraextend_clean', {
    A_fit <- A_fit_from_80_20_split(hand4_ultraextend_clean, muscles_of_interest, force_names_to_predict)
    generator_columns_A_matrix <- t(t(A_fit$AMatrix) %*% diag(num_muscles))
    compute_ranks_of_A(A_fit$AMatrix)
    # Here, identify a force vector of interest and apply it to the generated A
    binary_combinations <- custom_binary_combinations(num_muscles,range_tension)
    binary_combination_ffs_points <- binary_combinations %*% generator_columns_A_matrix
    aspect3d(1/5,1/5,1/5); par3d(windowRect=c(0,0,10000,10000))
    plot_ffs_with_vertices(binary_combination_ffs_points[,1:3], generator_columns_A_matrix[,1:3], alpha_transparency=0.25, range_tension=range_tension)
    points3d(input_output_data[,force_names_to_predict][,1:3], size=1, col="black", alpha=1)
    title3d(main="FFS", xlab="Fx", ylab="Fy", zlab="Fz", col="black")
})
