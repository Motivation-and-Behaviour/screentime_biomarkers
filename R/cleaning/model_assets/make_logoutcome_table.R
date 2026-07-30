#' Sensitivity 4 results table with log-scale coefficients back-transformed via exp().
#'
#' Uses flextable directly because `gtsummary::tbl_regression(exponentiate = TRUE)`
#' is not supported for lavaan models.
#'
#' @param logoutcome_dfs data.frame. Combined tidy estimates from the
#'   log-transformed-outcome models (columns outcome, term, coef, ci_l, ci_u,
#'   pvalue, type).
#' @param caption character. Table caption.
#'
#' @return A flextable.
#' @export
make_logoutcome_table <- function(logoutcome_dfs,
                                  caption = paste(
                                    "Sensitivity analysis 4. Associations between",
                                    "screen-time trajectories and right-skewed",
                                    "outcomes modelled on the natural-log scale,",
                                    "back-transformed to a multiplicative change",
                                    "(ratio) per SD of screen time."
                                  )) {
  require(flextable)

  labels <- c(
    "ApoBA1_ratio_w6.5" = "ApoB/ApoA1 Ratio",
    "glycoprotein_w6.5" = "Glycoprotein Acetyls",
    "trigly_w6.5" = "Triglycerides",
    "glucose_w6.5" = "Glucose",
    "waistcm_w6.5" = "Waist Circumference",
    "waist2height_w6.5" = "Waist-to-Height Ratio"
  )
  term_labels <- c(
    st_intercept = "Screen Time Trajectory Intercept",
    st_slope = "Screen Time Trajectory Slope"
  )

  d <- as.data.frame(logoutcome_dfs)
  d <- d[d$term %in% names(term_labels), ]
  d$cell <- sprintf(
    "%.2f [%.2f, %.2f]", exp(d$coef), exp(d$ci_l), exp(d$ci_u)
  )
  d$Outcome <- ifelse(d$outcome %in% names(labels),
    labels[d$outcome], d$outcome
  )
  d$Term <- term_labels[d$term]

  wide <- d[, c("Outcome", "Term", "type", "cell")] |>
    tidyr::pivot_wider(names_from = type, values_from = cell)

  # Keep a stable column order even if a type is missing.
  for (nm in c("unadjusted", "adjusted")) {
    if (!nm %in% names(wide)) wide[[nm]] <- NA_character_
  }
  wide <- wide[, c("Outcome", "Term", "unadjusted", "adjusted")]

  flextable::flextable(wide) |>
    flextable::set_header_labels(
      unadjusted = "Unadjusted", adjusted = "Adjusted"
    ) |>
    flextable::set_caption(caption) |>
    flextable::merge_v(j = "Outcome") |>
    flextable::valign(j = "Outcome", valign = "top") |>
    flextable::add_footer_lines(paste(
      "Estimates are the multiplicative change in the outcome (ratio) per 1 SD",
      "of screen time, back-transformed from log-scale coefficients [95% CI].",
      "A ratio of 1.00 indicates no association."
    )) |>
    flextable::set_table_properties(layout = "autofit")
}
