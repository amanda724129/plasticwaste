#' GDP and Plastics Summary Table
#'
#' Creates a summary table of the countries with the highest plastic waste,
#' displaying each country's total plastic waste and average GDP per capita.
#'
#' @param top_x_countries the number of top countries the user wants to display
#'
#' @return A data frame containing the top countries by total plastic waste,
#' along with their total plastic waste and average GDP per capita.#'
#'
#' @export
#'
#' @importFrom furrr future_map
#'
#' @examples
#' gdp_plastics_table(5)
gdp_plastics_table <- function(top_x_countries = 10) {

  stopifnot(is.numeric(top_x_countries),
            length(top_x_countries) == 1,
            top_x_countries > 0)

  plastics_top <- load_data()

  plastics_top |>
    distinct(country) |>
    filter(!country %in% c("EMPTY", "Taiwan_ Republic of China (ROC)", "Nigeria")) |>
    mutate(
      result = future_map(country, gdp_plastics),
      country = recode(country, "NIGERIA" = "Nigeria", "ECUADOR" = "Ecuador")
    ) |>
    unnest(result) |>
    slice_max(plastic_total, n = top_x_countries)
}
