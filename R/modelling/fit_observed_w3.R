#' Sensitivity 1: replaces the latent intercept with observed `st_totalz_w3`.
#' Also handles period-average exposures (st_earlyz, st_latez) via `exposure`.
#'
#' @param transformed_data data.table. The wide analysis data (one row per id).
#' @param outcome character. The outcome variable name.
#' @param bloods logical. Is the outcome from a blood test (adds fasting time)?
#' @param exposure character. The single screen-time exposure column to use in
#'   place of the latent intercept. Defaults to the observed Wave 3 value; also
#'   used for the Waves 3-4 (`st_earlyz`) and Waves 5-6 (`st_latez`)
#'   period-average exposures in the definition-change sensitivity analysis.
#' @return A list of two `lm` objects (`w3_fit`, `w3_adj_fit`) with an
#'   `"outcome"` attribute, mirroring the shape returned by [fit_lgcm()].
#' @author Taren Sanders
#' @export
fit_observed_w3 <- function(transformed_data, outcome, bloods,
                            exposure = "st_totalz_w3") {
  covariates_v <- c(
    exposure, # observed screen-time exposure, in place of latent intercept
    "female", "indig", "ses_w6", "bad_diet", "sexualmaturity_numeric_w6.5"
  )
  if (bloods) {
    covariates_v <- c(covariates_v, "fastingtime_w6.5")
  }

  data <- as.data.frame(transformed_data)

  w3_fit <- stats::lm(
    stats::reformulate(covariates_v, response = outcome),
    data = data
  )

  adj_covariates_v <- c(covariates_v, "accmvpa_w6.5_scaled", "accsed_w6.5_scaled")
  w3_adj_fit <- stats::lm(
    stats::reformulate(adj_covariates_v, response = outcome),
    data = data
  )

  model_outputs <- list(w3_fit = w3_fit, w3_adj_fit = w3_adj_fit)
  attr(model_outputs, "outcome") <- outcome
  model_outputs
}
