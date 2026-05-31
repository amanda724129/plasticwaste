test_that("Sankey plot works", {
  sankey_plot_simple <- sankey_plot(top_x_countries = 2, top_x_companies = 2)

  sankey_plot_complex <- sankey_plot(top_x_countries = 10, top_x_companies = 10)

  expect_s3_class(sankey_plot_simple, "ggplot")
  expect_s3_class(sankey_plot_complex, "ggplot")
})
