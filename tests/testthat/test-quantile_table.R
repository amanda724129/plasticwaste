test_that("quantile table works", {
  result <- quantile_table()

  ## Check the type of the object
  expect_s3_class(result, "tbl_df")

  ## Check the column names
  expect_equal(names(result), c("gdp_category", "total_plastic", "avg_gdp_per_capita", "share_of_total"))

  ## Check the type of columns
  expect_type(result$gdp_category, "character")
  expect_type(result$total_plastic, "integer")
  expect_type(result$avg_gdp_per_capita, "double")
  expect_type(result$share_of_total, "double")

  ## Check the values of table
  expect_equal(result$total_plastic[1], 353689)
  expect_equal(round(result$avg_gdp_per_capita[1], digits = 2), 1848.47)
})
