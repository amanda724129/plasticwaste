#' Create a quantile summary table for plastic waste by GDP category
#'
#' Classifies countries into three GDP quantile groups (Low, Mid, High) based on
#' GDP per capita, then summarises total plastic waste, average GDP per capita,
#' and each group's share of the overall total for each group.
#'
#' @param lower_quantile A quantile that represents a percentage of the data, defining the Low/Mid boundary. Must be between 0 and 1 and defaults to 0.33.
#' @param upper_quantile A quantile that represents a percentage of the data, defining the Mid/High boundary. Must be between 0 and 1, greater than the lower quantile, and defaults to 0.66.
#'
#' @return A tibble with four columns: gdp_category (the quantile group label),
#' total_plastic (total plastic waste for the group),
#' avg_gdp_per_capita (average GDP per capita for the group), and
#' share_of_total (the group's share of overall plastic waste).
#'
#' @importFrom dplyr distinct mutate filter group_by summarise pull
#' @importFrom furrr future_map_int future_map_dbl
#'
#' @export

quantile_table <- function(lower_quantile = 0.33, upper_quantile = 0.66) {
  stopifnot(
    is.numeric(lower_quantile), lower_quantile > 0, lower_quantile < 1,
    is.numeric(upper_quantile), upper_quantile > 0, upper_quantile < 1,
    lower_quantile < upper_quantile
  )

  plastics_top <- load_data()
  labels <- c("Low GDP", "Mid GDP", "High GDP")

  classified <- plastics_top |>
    distinct(country, year, grand_total, gdp_per_capita_nominal) |>
    mutate(
      gdp_category = dplyr::case_when(
        gdp_per_capita_nominal <= quantile(gdp_per_capita_nominal, lower_quantile, na.rm = TRUE) ~ "Low GDP",
        gdp_per_capita_nominal <= quantile(gdp_per_capita_nominal, upper_quantile, na.rm = TRUE) ~ "Mid GDP",
        TRUE ~ "High GDP"),
      gdp_category = factor(gdp_category, levels = labels))

  quantile_plastic <- function(input_quantile) {
    classified |>
      filter(gdp_category == input_quantile) |>
      pull(grand_total) |>
      sum(na.rm = TRUE)
  }

  quantile_gdp <- function(input_quantile) {
    classified |>
      filter(gdp_category == input_quantile) |>
      group_by(country) |>
      summarise(avg_gdp = mean(gdp_per_capita_nominal, na.rm = TRUE), .groups = "drop") |>
      pull(avg_gdp) |>
      mean(na.rm = TRUE)
  }

  tibble::tibble(gdp_category = labels) |>
    mutate(
      total_plastic = future_map_int(gdp_category, quantile_plastic),
      avg_gdp_per_capita = future_map_dbl(gdp_category, quantile_gdp),
      share_of_total = total_plastic / sum(total_plastic, na.rm = TRUE))
}
