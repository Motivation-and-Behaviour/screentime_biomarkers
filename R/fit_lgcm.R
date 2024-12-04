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
  covariates <- "sex_w6.5 + indig_w6 + ses_w6 + diet_w6.5 + sexualmaturity_w6.5"
  if (bloods) {
    covariates <- glue::glue("{covariates} + fastingtime_w6.5")
  }

  model <- glue::glue(
    # nolint start
    "
    # LGCM
    intercept =~ 1 * st_total_w3 + 1 * st_total_w4 + 1 * st_total_w5 + 1 * st_total_w6
    slope =~ 0 * st_total_w3 + 1 * st_total_w4 + 2 * st_total_w5 + 3 * st_total_w6

    # Variances and covariances
    intercept ~~ intercept
    slope ~~ slope
    intercept ~~ slope

    # Residual variances
    st_total_w3 ~~ residual_var*st_total_w3
    st_total_w4 ~~ residual_var*st_total_w4
    st_total_w5 ~~ residual_var*st_total_w5
    st_total_w6 ~~ residual_var*st_total_w6
    residual_var > 0

    # Regression of health outcome on latent factors and covariates
    {outcome} ~ intercept + slope + {covariates}
    "
    # nolint end
  )

  adj_model <- glue::glue(
    # nolint start
    "
    # LGCM
    intercept =~ 1 * st_total_w3 + 1 * st_total_w4 + 1 * st_total_w5 + 1 * st_total_w6
    slope =~ 0 * st_total_w3 + 1 * st_total_w4 + 2 * st_total_w5 + 3 * st_total_w6

    # Variances and covariances
    intercept ~~ intercept
    slope ~~ slope
    intercept ~~ slope

    # Residual variances
    st_total_w3 ~~ residual_var*st_total_w3
    st_total_w4 ~~ residual_var*st_total_w4
    st_total_w5 ~~ residual_var*st_total_w5
    st_total_w6 ~~ residual_var*st_total_w6
    residual_var > 0

    # Regression of health outcome on latent factors and covariates
    {outcome} ~ intercept + slope + {covariates}

    {outcome} ~ accmvpa_w6.5 + accsed_w6.5
    "
    # nolint end
  )
  model_outputs <- list()

  model_outputs$lgcm_fit <- lavaan::growth(
    model,
    data = transformed_data, missing = "fiml"
  )
  model_outputs$lgcm_adj_fit <- lavaan::growth(
    adj_model,
    data = transformed_data, missing = "fiml"
  )

  model_outputs
}
