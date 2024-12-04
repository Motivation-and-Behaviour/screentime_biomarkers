#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param transformed_data
#' @param outcome
#' @return
#' @author {Taren Sanders}
#' @export
fit_lgcm <- function(transformed_data) {
  require(lavaan)
  model <- glue::glue(
    # nolint start
    "
    # LGCM
    intercept =~ 1 * st_total_w3 + 1 * st_total_w4 + 1 * st_total_w5 + 1 * st_total_w6
    slope =~ 0 * st_total_w3 + 1 * st_total_w4 + 2 * st_total_w5 + 3 * st_total_w6
    
    bpsys_w6.5 ~ intercept + slope
    "
    # nolint end
  )

  transformed_data <-
    transformed_data %>%
    dplyr::filter(!is.na(st_total_w3) & !is.na(st_total_w4) & !is.na(st_total_w5) & !is.na(st_total_w6))

  lgcm_fit <- lavaan::growth(model, data = transformed_data)

  lgcm_fit
}
