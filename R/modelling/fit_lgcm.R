#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param transformed_data df. The transformed data.
#' @param outcome character. The outcome variable.
#' @param bloods boolean. Is the outcome from a blood test?
#' @return
#' @author {Taren Sanders}
#' @export
fit_lgcm <- function(transformed_data, outcome, bloods) {
  require(lavaan)
  covariates <- "female + indig + ses_w6 + bad_diet + sexualmaturity_numeric_w6.5"
  if (bloods) {
    covariates <- glue::glue("{covariates} + fastingtime_w6.5")
  }

  model <- glue::glue(
    # nolint start
    "
    # lgcm
    st_intercept =~ 1 * st_totalz_w3 + 1 * st_totalz_w4 + 1 * st_totalz_w5 + 1 * st_totalz_w6
    st_slope =~ 0 * st_totalz_w3 + 1 * st_totalz_w4 + 2 * st_totalz_w5 + 3 * st_totalz_w6

    # variances and covariances
    st_intercept ~~ st_intercept
    st_slope ~~ st_slope
    st_intercept ~~ st_slope

    # residual variances
    st_totalz_w3 ~~ residual_var*st_totalz_w3
    st_totalz_w4 ~~ residual_var*st_totalz_w4
    st_totalz_w5 ~~ residual_var*st_totalz_w5
    st_totalz_w6 ~~ residual_var*st_totalz_w6
    residual_var > 0

    # regression of health outcome on latent factors and covariates
    {outcome} ~ 1 + st_intercept + st_slope + {covariates}
    st_intercept ~ 1 + ses_w6 + female + indig + sexualmaturity_numeric_w6.5 # starting values depend on basic demographics
    "
    # nolint end
  )

  adj_model <- glue::glue(
    "{model}
    {outcome} ~ accmvpa_w6.5_scaled + accsed_w6.5_scaled"
  )

  model_outputs <- list()

  model_outputs$lgcm_fit <- lavaan::growth(
    model,
    data = transformed_data, missing = "fiml"
  )
  mi <- lavaan::modindices(model_outputs$lgcm_fit)
  mi[order(mi$mi, decreasing = TRUE), ][1:10, ]
  fitMeasures(model_outputs$lgcm_fit, c("rmsea", "cfi", "tli", "bic", "aic"))

  model_outputs$lgcm_adj_fit <- lavaan::growth(
    adj_model,
    data = transformed_data, missing = "fiml"
  )

  model_outputs
}
