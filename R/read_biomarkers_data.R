#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param filepath
read_biomarkers_data <- function(filepath) {

  foreign::read.spss(filepath, to.data.frame = TRUE, use.value.labels = TRUE)

}
