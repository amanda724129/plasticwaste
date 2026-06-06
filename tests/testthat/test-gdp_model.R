test_that("gdp_model works", {
  result <- gdp_model()

  expect_s3_class(result, "summary.lm")
})
