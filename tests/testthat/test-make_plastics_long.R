test_that("make_plastics_long returns correct structure", {

  result <- make_plastics_long()

  # Dimensions
  expect_equal(nrow(result), 12060)
  expect_equal(ncol(result), 6)

  # Column types
  expect_type(result$country, "character")
  expect_type(result$total_plastic, "double")
  expect_type(result$year, "double")
  expect_type(result$plastic_type, "character")
  expect_type(result$parent_company, "character")

})
