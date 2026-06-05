#' Sensitivity 2 (stage 1): random intercept/slope LMM of screen-time
#' trajectories.
#' Time coded 0-3 (Waves 3-6) to match [fit_lgcm()] slope loadings.
#' BLUPs passed to [fit_lmm_outcome()] for stage 2.
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
  long[, time := match(wave, st_vars) - 1L]

  model <- lme4::lmer(
    st_totalz ~ time + (time | id),
    data = long,
    REML = TRUE
  )

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
