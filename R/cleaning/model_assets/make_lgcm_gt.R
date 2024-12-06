make_lgcm_gt <- function(model, variable, model_fit, main = TRUE) {
  require(gtsummary)

  theme_gtsummary_journal("jama", set_theme = TRUE)

  unadj <- make_gt_table(model$lgcm_fit, variable, main = main)
  adj <- make_gt_table(model$lgcm_adj_fit, variable, main = main)

  merged_tbl <- tbl_merge(list(unadj, adj),
    tab_spanner = c("**Unadjusted**", "**Adjusted**")
  )

  merged_tbl
}

make_gt_table <- function(model_fit, variable, main = TRUE) {
  names_map <- list(
    "cardio_index_w6.5" = "Cardio-metabolic Risk Score",
    "ApoBA1_ratio_w6.5" = "ApoB/ApoA1 Ratio",
    "glycoprotein_w6.5" = "Glycoprotein Acetyls",
    "phospholipids_w6.5" = "Phospholipids ",
    "vo2_w6.5" = "Cardiorespiratory Fitness (VO2 Max)",
    "waistcm_w6.5" = "Waist Circumference",
    "waist2height_w6.5" = "Waist-to-Height Ratio",
    "bmiz_w6.5" = "Body Mass Index (z-score)",
    "bodyfat_w6.5" = "Body Fat Percentage",
    "bpsysamp_w6.5" = "Systolic Blood Pressure Ampification",
    "pulsepressamp_w6.5" = "Pulse Pressure Amplification",
    "bpsysz_w6.5" = "Sysolic Blood Pressure (z-score)",
    "bpdiaz_w6.5" = "Diastolic Blood Pressure (z-score)",
    "trigly_w6.5" = "Triglycerides",
    "cholesttotal_w6.5" = "Total Cholesterol",
    "cholesttotalhdl_w6.5" = "HDL Cholesterol",
    "cholestnonhdl_w6.5" = "Non-HDL Cholesterol",
    "glucose_w6.5" = "Glucose"
  )

  terms_map <- c(
    "Screen Time Trajectory Intercept",
    "Screen Time Trajectory Slope",
    "Female",
    "Indigenous",
    "Socioeconomic Status",
    "Does Not Meet Dietary Guidelines",
    "Sexual Maturity Stage",
    "Fasting Time",
    "Moderate-to-Vigorous Physical Activity",
    "Sedentary Time"
  )

  names(terms_map) <- c(
    glue::glue("{variable} ~ st_intercept"),
    glue::glue("{variable} ~ st_slope"),
    glue::glue("{variable} ~ female"),
    glue::glue("{variable} ~ indig"),
    glue::glue("{variable} ~ ses_w6"),
    glue::glue("{variable} ~ bad_diet"),
    glue::glue("{variable} ~ sexualmaturity_numeric_w6.5"),
    glue::glue("{variable} ~ fastingtime_w6.5"),
    glue::glue("{variable} ~ accmvpa_w6.5_scaled"),
    glue::glue("{variable} ~ accsed_w6.5_scaled")
  )

  remove_vec <- c(
    glue::glue("{variable} ~1 "),
    glue::glue("{variable} ~~ {variable}")
  )

  if (main) {
    remove_vec <- c(remove_vec, terms_map[3:8])
  }

  base_table <-
    tbl_regression(model_fit, include = c(variable), label = names_map) |>
    bold_p() |>
    bold_labels() |>
    modify_table_body(dplyr::mutate, label = dplyr::recode(label, !!!terms_map)) |>
    modify_table_body(dplyr::filter, !label %in% remove_vec) |>
    modify_header(label = "**Variable**")
}
