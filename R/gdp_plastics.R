#' GDP Plastics Summary Function
#'
#' Calculates the total plastic and the average GDP per capita for each country
#'
#' @param input_country Country that is getting the totals calculated for
#'
#' @return A tibble containing the plastic total and the average GDP per capita for the input country


gdp_plastics <- function(input_country){

  plastics_top <- load_data()

  country_data <- plastics_top |>
    filter(country == input_country) |>
    distinct(year, grand_total, gdp_per_capita_nominal)

  tibble(
    plastic_total = sum(country_data$grand_total, na.rm = TRUE),
    avg_gdp_per_capita_nominal = mean(country_data$gdp_per_capita_nominal, na.rm = TRUE)
  )
}
