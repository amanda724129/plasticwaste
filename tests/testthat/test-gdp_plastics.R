test_that("gdp_plastics function works", {
  result <- gdp_plastics("Nigeria")

  expect_s3_class(result, "tbl_df")
})
