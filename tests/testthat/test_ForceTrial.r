
context("test_ForceTrial")

test_that("df_to_max_signed_residual gets the correct ranges", {
  one_ft <- readRDS(all_file_paths("~/Resilio Sync/data/ForceTrials_at_each_posture/")[1])[[1]]
  force_df <- ft_to_df(one_ft)
  residual <- df_to_max_signed_residual(force_df)
  expect_equal(residual, -0.144445)
  expect_equal(abs(residual), attr(one_ft,'stability_info')[['max_residual']])
})
