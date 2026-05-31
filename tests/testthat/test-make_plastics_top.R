test_that("make_plastics_top returns correct structure", {

  result <- make_plastics_top()

  # Dimensions
  expect_equal(nrow(result), 11157)
  expect_equal(ncol(result), 21)

  # Column types
  expect_type(result$country, "character")
  expect_type(result$grand_total, "double")
  expect_type(result$gdp_per_capita_nominal, "double")

})
