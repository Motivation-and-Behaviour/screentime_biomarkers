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
  model <- glue::glue(
    # nolint start
    "
    # LGCM
    intercept =~ 1 * st_total_w3 + 1 * st_total_w4 + 1 * st_total_w5 + 1 * st_total_w6
    slope =~ 0 * st_total_w3 + 1 * st_total_w4 + 2 * st_total_w5 + 3 * st_total_w6
    "
    # nolint end
  )

  transformed_data <-
    transformed_data %>%
    filter(!is.na(st_total_w3) & !is.na(st_total_w4) & !is.na(st_total_w5) & !is.na(st_total_w6))

  lgcm_fit <- lavaan::growth(model, data = transformed_data)
  lavInspect(lgcm_fit, "fit")
  lavInspect(lgcm_fit, "cov.lv")

  factor_scores <- lavaan::lavPredict(lgcm_fit)

  # Add factor scores to the data
  transformed_data$intercept_factor <- factor_scores[, 1]
  transformed_data$slope_factor <- factor_scores[, 2]

  # Regress health outcome on the growth factors and covariates
  model_with_covariates <- "
    waistcm_w6.5 ~ intercept_factor + slope_factor + sex + indig + ses_w6
  "

  # Fit the model
  fit_with_covariates <- lavaan::sem(model_with_covariates, data = transformed_data)

  # Summary
  summary(fit_with_covariates, fit.measures = TRUE, standardized = TRUE)
}
