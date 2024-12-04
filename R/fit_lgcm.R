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
  covariates <- "sex + indig_w6 + ses + diet + sexualmaturity_numeric_w6.5"
  if (bloods) {
    covariates <- glue::glue("{covariates} + fastingtime_w6.5")
  }
browser()

  model <- glue::glue(
    # nolint start
    "
    # LGCM
    intercept =~ 1 * st_total_w3_scaled + 1 * st_total_w4_scaled + 1 * st_total_w5_scaled + 1 * st_total_w6_scaled
    slope =~ 0 * st_total_w3_scaled + 1 * st_total_w4_scaled + 2 * st_total_w5_scaled + 3 * st_total_w6_scaled

    # Variances and covariances
    intercept ~~ intercept
    slope ~~ slope
    intercept ~~ slope

    # Residual variances
    st_total_w3_scaled ~~ residual_var*st_total_w3_scaled
    st_total_w4_scaled ~~ residual_var*st_total_w4_scaled
    st_total_w5_scaled ~~ residual_var*st_total_w5_scaled
    st_total_w6_scaled ~~ residual_var*st_total_w6_scaled
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
    intercept =~ 1 * st_total_w3_scaled + 1 * st_total_w4_scaled + 1 * st_total_w5_scaled + 1 * st_total_w6_scaled
    slope =~ 0 * st_total_w3_scaled + 1 * st_total_w4_scaled + 2 * st_total_w5_scaled + 3 * st_total_w6_scaled

    # Variances and covariances
    intercept ~~ intercept
    slope ~~ slope
    intercept ~~ slope

    # Residual variances
    st_total_w3_scaled ~~ residual_var*st_total_w3_scaled
    st_total_w4_scaled ~~ residual_var*st_total_w4_scaled
    st_total_w5_scaled ~~ residual_var*st_total_w5_scaled
    st_total_w6_scaled ~~ residual_var*st_total_w6_scaled
    residual_var > 0

    # Regression of health outcome on latent factors and covariates
    {outcome} ~ intercept + slope + {covariates}

    {outcome} ~ accmvpa_w6.5_scaled + accsed_w6.5_scaled
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
