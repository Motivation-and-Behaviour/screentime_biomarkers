#' Sensitivity analysis 2 (stage 2): outcomes on mixed-model trajectories
#'
#' Regresses each health outcome on the subject-specific intercept and slope
#' estimated by the `lme4` trajectory model ([fit_st_trajectory_lmm()]), using
#' the same covariate adjustment structure as the latent growth curve model.
#' This provides a mixed-model analog of the main SEM analysis. Models are
#' ordinary least squares (listwise deletion).
#'
#' @param st_trajectory_lmm list. Output of [fit_st_trajectory_lmm()] (must
#'   contain `blups`).
#' @param transformed_data data.table. The wide analysis data (one row per id).
#' @param outcome character. The outcome variable name.
#' @param bloods logical. Is the outcome from a blood test (adds fasting time)?
#' @return A list of two `lm` objects (`lmm_fit`, `lmm_adj_fit`) with an
#'   `"outcome"` attribute, mirroring the shape returned by [fit_lgcm()].
#' @author Taren Sanders
#' @export
fit_lmm_outcome <- function(st_trajectory_lmm, transformed_data, outcome, bloods) {
  data <- merge(
    as.data.frame(transformed_data),
    st_trajectory_lmm$blups,
    by = "id",
    all.x = TRUE
  )

  # blup_intercept / blup_slope replace the latent intercept / slope; remaining
  # covariates match the main latent growth curve model (fit_lgcm.R)
  covariates_v <- c(
    "blup_intercept", "blup_slope",
    "female", "indig", "ses_w6", "bad_diet", "sexualmaturity_numeric_w6.5"
  )
  if (bloods) {
    covariates_v <- c(covariates_v, "fastingtime_w6.5")
  }

  lmm_fit <- stats::lm(
    stats::reformulate(covariates_v, response = outcome),
    data = data
  )

  adj_covariates_v <- c(covariates_v, "accmvpa_w6.5_scaled", "accsed_w6.5_scaled")
  lmm_adj_fit <- stats::lm(
    stats::reformulate(adj_covariates_v, response = outcome),
    data = data
  )

  model_outputs <- list(lmm_fit = lmm_fit, lmm_adj_fit = lmm_adj_fit)
  attr(model_outputs, "outcome") <- outcome
  model_outputs
}
