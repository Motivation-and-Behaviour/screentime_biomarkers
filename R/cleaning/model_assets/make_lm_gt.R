lm_names_map <- function() {
  list(
    "cardio_index_w6.5" = "Cardio-metabolic Risk Score",
    "ApoBA1_ratio_w6.5" = "ApoB/ApoA1 Ratio",
    "glycoprotein_w6.5" = "Glycoprotein Acetyls",
    "phospholipids_w6.5" = "Phospholipids",
    "vo2_w6.5" = "Cardiorespiratory Fitness (VO2 Max)",
    "waistcm_w6.5" = "Waist Circumference",
    "waist2height_w6.5" = "Waist-to-Height Ratio",
    "bmiz_w6.5" = "Body Mass Index (z-score)",
    "bodyfat_w6.5" = "Body Fat Percentage",
    "bpsysamp_w6.5" = "Systolic Blood Pressure Amplification",
    "pulsepressamp_w6.5" = "Pulse Pressure Amplification",
    "bpsysz_w6.5" = "Systolic Blood Pressure (z-score)",
    "bpdiaz_w6.5" = "Diastolic Blood Pressure (z-score)",
    "trigly_w6.5" = "Triglycerides",
    "cholesttotal_w6.5" = "Total Cholesterol",
    "cholesttotalhdl_w6.5" = "HDL Cholesterol",
    "cholestnonhdl_w6.5" = "Non-HDL Cholesterol",
    "glucose_w6.5" = "Glucose"
  )
}

#' Merged unadjusted/adjusted gtsummary table for a sensitivity lm pair.
#'
#' @param model_pair list. Two `lm` objects (unadjusted, adjusted).
#' @param variable character. The outcome variable name.
#' @param exposure_terms named character. Names are model term names to keep,
#'   values are the friendly labels to display.
#' @return A `gtsummary` `tbl_merge` object.
#' @export
make_lm_gt <- function(model_pair, variable, exposure_terms) {
  require(gtsummary)
  theme_gtsummary_journal("jama", set_theme = TRUE)

  label_list <- stats::setNames(as.list(unname(exposure_terms)), names(exposure_terms))

  make_one <- function(fit) {
    tbl_regression(
      fit,
      include = dplyr::all_of(names(exposure_terms)),
      label = label_list
    ) |>
      bold_p() |>
      bold_labels()
  }

  unadj <- make_one(model_pair[[1]])
  adj <- make_one(model_pair[[2]])

  merged_tbl <- tbl_merge(
    list(unadj, adj),
    tab_spanner = c("**Unadjusted**", "**Adjusted**")
  )

  attr(merged_tbl, "outcome_label") <- lm_names_map()[[variable]] %||% variable
  merged_tbl
}

#' Stack per-outcome sensitivity tables into one table
#'
#' @param ... `gtsummary` tables from [make_lm_gt()].
#' @param caption character. Table caption.
#' @return A stacked `gtsummary` table grouped by outcome.
#' @export
make_sensitivity_table <- function(..., caption) {
  require(gtsummary)
  theme_gtsummary_journal("jama", set_theme = TRUE)

  tbls <- list(...)
  headers <- vapply(
    tbls,
    function(t) attr(t, "outcome_label") %||% "",
    character(1)
  )

  tbl_stack(tbls, group_header = headers) |>
    modify_caption(caption)
}

#' Tidy a sensitivity lm pair into a long coefficient data.frame
#'
#' @param model_pair list. Two `lm` objects (unadjusted, adjusted).
#' @return data.frame of coefficients with `outcome` and `type` columns.
#' @export
tidy_lm_pair <- function(model_pair) {
  outcome <- attr(model_pair, "outcome")
  types <- c("unadjusted", "adjusted")
  lapply(seq_along(model_pair), function(i) {
    broom::tidy(model_pair[[i]], conf.int = TRUE) |>
      dplyr::mutate(outcome = outcome, type = types[i])
  }) |>
    dplyr::bind_rows()
}

#' Write a sensitivity-analysis coefficient table to CSV
#'
#' @param model_dfs data.frame. Combined output of [tidy_lm_pair()].
#' @param outpath character. File path for the CSV.
#' @return The output path (for `format = "file"` targets).
#' @export
make_lm_diagnostic_table <- function(model_dfs, outpath) {
  model_dfs |>
    dplyr::transmute(
      Outcome = outcome,
      Term = term,
      Coef = glue::glue(
        "{round(estimate, 2)} [{round(conf.low, 2)}, {round(conf.high, 2)}]"
      ),
      SE = round(std.error, 2),
      t = round(statistic, 2),
      p = metaKIN::round_p(p.value),
      type = type
    ) |>
    data.table::fwrite(outpath)

  outpath
}
