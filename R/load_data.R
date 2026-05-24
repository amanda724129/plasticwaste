#' Loads in the plastic data
#'
#' @param csv_data The csv data set that is getting read into the global environment
#'
#' @return The csv data set as a data frame
#'
#' @importFrom readr read_csv
#'
#'@export
load_data <- function(csv_data){
  readr::read_csv(csv_data)
}
