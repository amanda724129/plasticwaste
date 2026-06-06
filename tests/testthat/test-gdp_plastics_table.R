test_that("gdp_plastics_table works", {

  result <- gdp_plastics_table(5)

  expect_s3_class(result, "data.frame")

  expect_equal(nrow(result), 5)
})
