#' Sensitivity analysis 2 (stage 1): mixed-model screen-time trajectory
#'
#' Fits a conventional longitudinal random intercept / random slope mixed model
#' to the standardised screen-time measures, analogous to the latent growth
#' curve model but in an `lme4` framework that will be more familiar to readers
#' used to mixed models. Time is coded 0, 1, 2, 3 across Waves 3-6 to match the
#' factor loadings of the latent slope in [fit_lgcm()].
#'
#' Subject-specific (BLUP) intercepts and slopes are extracted so that the
#' second stage ([fit_lmm_outcome()]) can relate the individual trajectories to
#' later health outcomes. These subject-specific estimates are shrunken towards
#' the population mean and do not propagate trajectory-estimation uncertainty;
#' this is acknowledged in the supplementary text.
#'
#' @param transformed_data data.table. The wide analysis data (one row per id).
#' @return A list with `model` (the fitted `lmerMod`) and `blups`
#'   (data.frame of `id`, `blup_intercept`, `blup_slope`).
#' @author Taren Sanders
#' @export
fit_st_trajectory_lmm <- function(transformed_data) {
  st_vars <- c("st_totalz_w3", "st_totalz_w4", "st_totalz_w5", "st_totalz_w6")

  long <- data.table::melt(
    data.table::as.data.table(transformed_data),
    id.vars = "id",
    measure.vars = st_vars,
    variable.name = "wave",
    value.name = "st_totalz"
  )
  # Map wave label -> time score (0,1,2,3), matching the LGM slope loadings
  long[, time := match(wave, st_vars) - 1L]

  model <- lme4::lmer(
    st_totalz ~ time + (time | id),
    data = long,
    REML = TRUE
  )

  # Subject-specific intercepts and slopes (fixed + random effect BLUPs)
  subj_coef <- stats::coef(model)$id
  blups <- data.frame(
    id = rownames(subj_coef),
    blup_intercept = subj_coef[["(Intercept)"]],
    blup_slope = subj_coef[["time"]],
    stringsAsFactors = FALSE
  )
  # Match the id type used in transformed_data for a clean downstream join
  blups$id <- methods::as(blups$id, class(transformed_data$id))

  list(model = model, blups = blups)
}
