make_lgcm_gt <- function(model, variable, model_fit, main = TRUE) {
  require(gtsummary)

  theme_gtsummary_journal("jama", set_theme = TRUE)

  names_map <- c(
    "cardio_index_w6.5" = "Cardio-metabolic Risk Score ",
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
    "trigly_w6.5" = "Triacylglycerides",
    "cholesttotal_w6.5" = "Total Cholesterol",
    "cholesttotalhdl_w6.5" = "HDL Cholesterol",
    "cholestnonhdl_w6.5" = "Non-HDL Cholesterol",
    "glucose_w6.5" = "Glucose"
  )


  # TODO: Fix the formatting of these
  unadj <-
    tbl_regression(model$lgcm_fit, include = c(variable, contains("st_intercept"))) |>
    bold_p() |>
    bold_labels()
  adj <- tbl_regression(model$lgcm_adj_fit, include = c(variable))

  merged_tbl <- tbl_merge(list(unadj, adj), tab_spanner = c("Unadjusted", "Adjusted"))

  merged_tbl
}


broom.helpers::tidy_plus_plus(model_cardio_index_w6.5$lgcm_fit)

x <- broom.helpers::tidy_plus_plus(model_cardio_index_w6.5$lgcm_fit)

x

tar_load(model_cardio_index_w6.5)
model <- model_cardio_index_w6.5
variable <- "cardio_index_w6.5"

x <- lavaan::parameterEstimates(model$lgcm_fit)
names(x)
x
