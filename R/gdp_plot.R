#' Creates a scatter plot
#'
#' Plots the relationship between average GDP per capita and plastic waste for each country
#'
#' @param cutoff1 the lowest cutoff for differentiating Low and Mid GDP
#' @param cutoff2 the highest cutoff for differentiating Mid and High GDP
#'
#' @return A scatterplot showing the relationship between average GDP per capita and plastic waste
#' coloring by GDP category and labelling the countries
#'
#' @import purrr
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr unnest
#' @importFrom countrycode countrycode
#'
#' @export

gdp_plot <- function(cutoff1 = 5000, cutoff2 = 20000) {

  stopifnot(
    is.numeric(cutoff1), cutoff1 > 0,
    is.numeric(cutoff2), cutoff2 > 0,
    cutoff1 < cutoff2
  )

  gdp_plastics <- function(input_country){

    country_data <- plastics_top |>
      filter(country == input_country) |>
      distinct(year, grand_total, gdp_per_capita_nominal)

    tibble(
      plastic_total = sum(country_data$grand_total, na.rm = TRUE),
      avg_gdp_per_capita_nominal = mean(country_data$gdp_per_capita_nominal, na.rm = TRUE)
    )
  }

  plastics_top <- make_plastics_top()

  plastics_top |>
    distinct(country) |>
    filter(!country %in% c("EMPTY", "Taiwan_ Republic of China (ROC)", "Nigeria")) |>
    mutate(
      result = map(country, gdp_plastics),
      country = recode(country, "NIGERIA" = "Nigeria", "ECUADOR" = "Ecuador")
    ) |>
    unnest(result) |>
    mutate(gdp_category = case_when(
      is.na(avg_gdp_per_capita_nominal) ~ "Unknown",
      avg_gdp_per_capita_nominal < cutoff1  ~ "Low GDP",
      avg_gdp_per_capita_nominal < cutoff2 ~ "Middle GDP",
      avg_gdp_per_capita_nominal >= cutoff2 ~ "High GDP"
    )) |>
    filter(country != "China") |>
    ggplot(aes(x = avg_gdp_per_capita_nominal, y = plastic_total, color = gdp_category)) +
    geom_point(size = 1.5) +
    ggrepel::geom_text_repel(
      aes(label = country),
      size = 2
    ) +
    labs(
      title = "Total Plastic Waste vs GDP by Country",
      subtitle = "Colored by GDP Category",
      x = "Avg GDP Per Capita(Nominal, USD)",
      y = "Total Plastic Waste",
      color = "GDP Category",
      caption = "Figure 4"
    ) +
    scale_color_manual(
      values = c(
        "Low GDP" = "#CD0200",
        "Middle GDP" = "#3CB521",
        "High GDP" = "#446E9B"
      ),
      breaks = c("High GDP", "Middle GDP", "Low GDP")
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12),
      plot.margin = margin(15, 15, 15, 15),
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0.45,
                                  size = 10,
                                  face = "bold")
    )
}
