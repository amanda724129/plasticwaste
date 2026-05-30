#' GDP and Plastics Summary Table
#'
#' Creates a summary table of the countries with the highest plastic waste,
#' displaying each country's total plastic waste and average GDP per capita.
#'
#' @param top_x_countries Integer. Number of countries to display.
#'
#' @return A data frame containing the top countries by total plastic waste,
#' along with their total plastic waste and average GDP per capita.#' @export
#'
#' @examples
#' gdp_plastics_table(5)
gdp_plastics_table <- function(top_x_countries) {

  if (!is.numeric(top_x_countries) ||
      length(top_x_countries) != 1 ||
      top_x_countries <= 0) {
    stop("top_x_countries must be a positive integer")
  }

  plastics_top %>%
    dplyr::distinct(country, year.x, grand_total, gdp_per_capita_nominal) %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      plastic_total = sum(grand_total, na.rm = TRUE),
      avg_gdp_per_capita_nominal = mean(gdp_per_capita_nominal, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(plastic_total)) %>%
    dplyr::slice_head(n = top_x_countries)
}

