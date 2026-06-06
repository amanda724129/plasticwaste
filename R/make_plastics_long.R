#' Create the plastics_long dataset
#'
#' Turns the plastics_top data into a long format so that a Sankey diagram can be made
#'
#' @return A long formatted version of plastics top,
#' with the total plastic, plastic type, country, year, and company being returned
#'
#' @importFrom dplyr filter distinct mutate pull summarise group_by left_join select
#' @importFrom tidyr pivot_longer

make_plastics_long <- function() {
  plastics_top <- load_data()

  plastics_top |>
    select(country, country_code, year, parent_company, hdpe, ldpe, o, pet, pp, ps, pvc) |>
    pivot_longer(
      cols = c(hdpe, ldpe, o, pet, pp, ps, pvc),
      names_to = "plastic_type",
      values_to = "total_plastic"
    ) |>
    filter(total_plastic > 0)
}
