plot_predictions <- function(model_predictions){
  require(ggplot2)
  model_predictions$outcome <- gsub("_", " ", model_predictions$outcome)
  model_predictions$outcome <- gsub(" w6.5", "", model_predictions$outcome)
  model_predictions$outcome <- Hmisc::capitalize(model_predictions$outcome)
  model_predictions <- model_predictions |>
  dplyr::mutate(
    outcome_recode = recode(outcome,
      "Cardio index"       = "Cardio Index",
      "ApoBA1 ratio"       = "ApoB/A1 Ratio",
      "Glycoprotein"       = "Glycoprotein",
      "Phospholipids"      = "Phospholipids",
      "Vo2"                = "VO₂ Max",
      "Waistcm"            = "Waist Circumference (cm)",
      "Waist2height"       = "Waist-to-Height Ratio",
      "Bmiz"               = "BMI z-score",
      "Bodyfat"            = "Body Fat (%)",
      "Bpsysamp"           = "Systolic Blood Pressure (Amp)",
      "Pulsepressamp"      = "Pulse Pressure (Amp)",
      "Bpsysz"             = "Systolic Blood Pressure z-score",
      "Bpdiaz"             = "Diastolic Blood Pressure",
      "Trigly"             = "Triglycerides",
      "Cholesttotal"       = "Total Cholesterol",
      "Cholesttotalhdl"    = "HDL Cholesterol",
      "Cholestnonhdl"      = "Non-HDL Cholesterol",
      "Glucose"            = "Glucose",
      .default = outcome  # Keeps original value if not matched
    )
  )

  fig_plot <- model_predictions |>
    ggplot(aes(x = st_vals, y = mean, color = type)) +
    geom_point() +
    geom_line() +
    facet_wrap(~outcome_recode, scales = "free_y") +
    theme_bw() +
    theme(text = element_text(family = "serif")) + 
    labs(color = "Model Type", x = "Screen Time", y = "Health Outcome")

  ggplot2::ggsave(fig_plot,
                  width = 35,
                  height = 20,
                  units = "cm",
                  filename = "outputs/model_predictions.png")
}
