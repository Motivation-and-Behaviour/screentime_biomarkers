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
  covariates <- "female + indig + ses_w6 + bad_diet + sexualmaturity_numeric_w6.5"
  if (bloods) {
    covariates <- glue::glue("{covariates} + fastingtime_w6.5")
  }

  model <- glue::glue(
    # nolint start
    "
    # lgcm
    intercept =~ 1 * st_total_w3_scaled + 1 * st_total_w4_scaled + 1 * st_total_w5_scaled + 1 * st_total_w6_scaled
    slope =~ 0 * st_total_w3_scaled + 1 * st_total_w4_scaled + 2 * st_total_w5_scaled + 3 * st_total_w6_scaled

    # variances and covariances
    intercept ~~ intercept
    slope ~~ slope
    intercept ~~ slope
    
    # include var intercepts
    bpsysamp_w6.5 ~ 1

    # residual variances
    st_total_w3_scaled ~~ residual_var*st_total_w3_scaled
    st_total_w4_scaled ~~ residual_var*st_total_w4_scaled
    st_total_w5_scaled ~~ residual_var*st_total_w5_scaled
    st_total_w6_scaled ~~ residual_var*st_total_w6_scaled
    residual_var > 0

    # regression of health outcome on latent factors and covariates
    {outcome} ~ intercept + slope + {covariates}
    intercept ~ ses_w6 + female + indig
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
  mi[order(mi$mi, decreasing = TRUE),]
  fitMeasures(model_outputs$lgcm_fit, c("rmsea", "cfi", "tli", "bic", "aic"))

  model_outputs$lgcm_adj_fit <- lavaan::growth(
    adj_model,
    data = transformed_data, missing = "fiml"
  )

  model_outputs
}
