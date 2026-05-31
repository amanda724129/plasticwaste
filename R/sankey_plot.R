#' Creates a Sankey plot
#'
#' Plots the relationship between country and company when it comes to plastic waste
#'
#' @param top_x_countries the number of top countries the user wants to display
#' @param top_x_companies the number of top companies the user wants to display
#'
#' @return A Sankey diagram showing the relationship between country and company for different types of
#' plastic waste
#'
#' @import purrr
#' @import dplyr
#' @import ggplot2
#' @import ggalluvial
#' @import paletteer
#' @importFrom countrycode countrycode
#'
#' @export

sankey_plot <- function(top_x_countries = 5, top_x_companies = 10) {

  plastics_top <- make_plastics_top()

  user_companies <- plastics_top |>
    distinct(parent_company, grand_total) |>
    filter(!is.na(parent_company),
           parent_company != "Grand Total",
           parent_company != "null",
           parent_company != "NULL",
           parent_company != "Unbranded") |>
    group_by(parent_company) |>
    summarise(plastic_total = sum(grand_total, na.rm = TRUE), .groups = "drop") |>
    slice_max(plastic_total, n = top_x_companies) |>
    pull(parent_company)

  country_totals <- plastics_top |>
    distinct(country, year, grand_total) |>
    group_by(country) |>
    summarise(plastic_total = sum(grand_total, na.rm = TRUE), .groups = "drop")

  user_countries <- plastics_top |>
    distinct(country, country_code) |>
    left_join(country_totals, by = "country") |>
    slice_max(plastic_total, n = top_x_countries) |>
    pull(country_code)

  plastics_long <- make_plastics_long()

  sankey_data <- plastics_long |>
    filter(parent_company %in% user_companies,
           country_code %in% user_countries)

  sankey_data |>
    mutate(
      country = forcats::fct_reorder(country,
                                     total_plastic,
                                     sum,
                                     .desc = TRUE),
      parent_company = forcats::fct_reorder(parent_company,
                                            total_plastic,
                                            sum,
                                            .desc = TRUE),
      parent_company = recode(parent_company,
                              "Universal Robina Corporation" = "Universal Robina",
                              "The Coca-Cola Company" = "Coca-Cola"),
      country = recode(country,
                       "NIGERIA" = "Nigeria")
    ) |>
    ggplot(aes(axis1 = country,
               axis2 = parent_company,
               y = total_plastic)) +
    scale_x_discrete(limits = c("Country", "Company"),
                     expand = c(.2, .05)) +
    geom_alluvium(aes(fill = plastic_type)) +
    geom_stratum() +
    ggfittext::geom_fit_text(stat = ggalluvial::StatStratum,
                             aes(label = after_stat(stratum)),
                             width = 1/4,
                             min.size = 3) +
    theme_bw() +
    scale_fill_paletteer_d("ggthemes::few_Dark") +
    xlab("Countries vs. Companies") +
    ylab("Total Plastic") +
    ggtitle("Plastic Waste from Country to Company",
            "Stratified by Plastic Type") +
    labs(fill = "Plastic Type",
         caption = "Figure 1") +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0.45,
                                      size = 10,
                                      face = "bold"))


}
