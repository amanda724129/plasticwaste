#' Create the plastics_top dataset
#'
#' Filters the raw plastics data to the top 50 countries by total plastic waste,
#' joins GDP per capita data, and removes countries with data quality issues.
#'
#' @return A data frame filtered to top plastic-producing countries with GDP data joined
#'
#' @importFrom dplyr filter distinct mutate pull summarise group_by left_join
#' @importFrom countrycode countrycode
make_plastics_top <- function() {
  plastics <- load_data()

  country_totals <- plastics |>
    distinct(country, year, grand_total) |>
    group_by(country) |>
    summarise(plastic_total = sum(grand_total, na.rm = TRUE), .groups = "drop")

  top_plastic_countries <- plastics |>
    distinct(country) |>
    filter(!country %in% c("EMPTY", "Taiwan_ Republic of China (ROC)")) |>
    left_join(country_totals, by = "country") |>
    mutate(country_code = countrycode(country,
                                      origin = "country.name",
                                      destination = "iso3c")) |>
    filter(!is.na(country_code)) |>
    filter(plastic_total != 61123) |>
    dplyr::slice_max(plastic_total, n = 50) |>
    pull(country_code)

  plastics |>
    filter(!country %in% c("EMPTY",
                           "Nigeria",
                           "Taiwan_ Republic of China (ROC)",
                           "United Kingdom",
                           "Montenegro")) |>
    mutate(country_code = countrycode(country,
                                      origin = "country.name",
                                      destination = "iso3c")) |>
    filter(country_code %in% top_plastic_countries) |>
    left_join(gdp_df, by = c("country_code", "year"))
}
