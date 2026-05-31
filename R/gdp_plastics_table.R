#' GDP and Plastics Summary Table
#'
#' Creates a summary table of the countries with the highest plastic waste,
#' displaying each country's total plastic waste and average GDP per capita.
#'
#' @param top_x_countries Integer. Number of countries to display.
#'
#' @return A data frame containing the top countries by total plastic waste,
#' along with their total plastic waste and average GDP per capita.#'
#' @export
#'
#' @examples
#' gdp_plastics_table(5)
gdp_plastics_table <- function(top_x_countries) {

  if (!is.numeric(top_x_countries) ||
      length(top_x_countries) != 1 ||
      top_x_countries <= 0) {
    stop("top_x_countries must be a positive integer")
  }

  plastics <- load_data()

  country_totals <- plastics |>
    distinct(country, year, grand_total) |>
    group_by(country) |>
    summarise(
      plastic_total = sum(grand_total, na.rm = TRUE),
      .groups = "drop"
    )

  top_plastic_countries <- plastics |>
    distinct(country) |>
    filter(!country %in% c("EMPTY", "Taiwan_ Republic of China (ROC)")) |>
    left_join(country_totals, by = "country") |>
    mutate(
      country_code = countrycode::countrycode(
        country,
        origin = "country.name",
        destination = "iso3c"
      )
    ) |>
    filter(!is.na(country_code)) |>
    filter(plastic_total != 61123) |>
    slice_max(plastic_total, n = 50) |>
    pull(country_code)

  plastics |>
    filter(!country %in% c(
      "EMPTY",
      "Nigeria",
      "Taiwan_ Republic of China (ROC)",
      "United Kingdom",
      "Montenegro"
    )) |>
    mutate(
      country_code = countrycode::countrycode(
        country,
        origin = "country.name",
        destination = "iso3c"
      )
    ) |>
    filter(country_code %in% top_plastic_countries) |>
    left_join(gdp_df, by = c("country_code", "year")) |>
    distinct(country, year, grand_total, gdp_per_capita_nominal) |>
    group_by(country) |>
    summarise(
      plastic_total = sum(grand_total, na.rm = TRUE),
      avg_gdp_per_capita_nominal = mean(gdp_per_capita_nominal, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(plastic_total)) |>
    slice_head(n = top_x_countries)
}

