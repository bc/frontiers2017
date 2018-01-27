
context("test_ForceTrial")

test_that("df_to_max_signed_residual gets the correct ranges", {
  one_ft <- read_rds_from_package_extdata("force_trial_adept_x_-527.463336_adept_y_68.rds")[[1]]
  force_df <- ft_to_df(one_ft)
  residual <- df_to_max_signed_residual(force_df)
  expect_equal(residual, -0.155, tol=1e-3)
  expect_equal(abs(residual), attr(one_ft,'stability_info')[['max_residual']])
})
