  context('test_forcetrial_stability.r')
  test_that('jr3_coordinate_transformation_along_z', {
    test_force_df <- data.frame(JR3_FX = 1.0, JR3_FY = 1.2, JR3_FZ=0.01, JR3_MX = 0.05, JR3_MY = 0.04, JR3_MZ = 0.03)
    result <- jr3_coordinate_transformation_along_z(test_force_df, 1.0)
    expect_equal(result[['JR3_MX']], test_force_df[['JR3_MX']] - 1.0*test_force_df[['JR3_FY']])
    expect_equal(result[['JR3_MY']], test_force_df[['JR3_MY']] - 1.0*test_force_df[['JR3_FX']])
  #with small distance
    result <- jr3_coordinate_transformation_along_z(test_force_df, 0.5)
    expect_equal(result[['JR3_MX']], test_force_df[['JR3_MX']] - 0.5*test_force_df[['JR3_FY']])
    expect_equal(result[['JR3_MY']], test_force_df[['JR3_MY']] - 0.5*test_force_df[['JR3_FX']])
  #with big distance
    result <- jr3_coordinate_transformation_along_z(test_force_df, 5)
    expect_equal(result[['JR3_MX']], test_force_df[['JR3_MX']] - 5*test_force_df[['JR3_FY']])
    expect_equal(result[['JR3_MY']], test_force_df[['JR3_MY']] - 5*test_force_df[['JR3_FX']])
  })
