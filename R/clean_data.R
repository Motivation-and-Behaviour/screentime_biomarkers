#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param waves_joined df. The survey data.
#' @param biomarkers_data df. The biomarkers data.
#' @param checkpoint_only logical. If TRUE, restrict data to only those present
#' at the checkpoint wave.
#' @return
#' @author {Taren Sanders}
#' @export
clean_data <- function(waves_joined, biomarkers_data, checkpoint_only = FALSE) {
  full_df <-
    dplyr::bind_rows(waves_joined, biomarkers_data)
}
