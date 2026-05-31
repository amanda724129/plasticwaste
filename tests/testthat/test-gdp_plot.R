test_that("GDP plot works", {

  gdp_plot_test <- gdp_plot(cutoff1 = 5000, cutoff2 = 20000)

  expect_s3_class(gdp_plot_test, "ggplot")
})
