#' Fit a linear model predicting total plastic waste
#'
#' @param explanatory_var A string giving the name of the explanatory variable to use in the model.
#' Must be a column in the data returned by load_data
#'
#' @return A linear model object of class lm
#' @export
#'
gdp_model <- function(explanatory_var = "gdp_per_capita_nominal") {

  if (!is.character(explanatory_var)) {
    stop("explanatory_var must be a character string")
  }

  plastics_top <- load_data()

  full_summary <- plastics_top |>
    distinct(country, year, grand_total) |>
    group_by(country) |>
    summarise(
      plastic_total = sum(grand_total, na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(plastics_top, by = "country")

  if (!explanatory_var %in% names(full_summary)) {
    stop(paste0("'", explanatory_var, "' is not a column in the data"))
  }

  formula <- as.formula(paste("plastic_total ~", explanatory_var))
  summary(lm(formula, data = full_summary))
}
