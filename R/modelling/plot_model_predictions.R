plot_predictions <- function(model_predictions){
  require(ggplot2)
  model_predictions$outcome <- gsub("_", " ", model_predictions$outcome)
  model_predictions$outcome <- gsub(" w6.5", "", model_predictions$outcome)

  model_predictions <- model_predictions |>
  dplyr::mutate(
    outcome_recode = recode(outcome,
      "cardio index"       = "Cardio index",
      "apoba1 ratio"       = "ApoB/A1 ratio",
      "glycoprotein"       = "Glycoprotein",
      "phospholipids"      = "Phospholipids",
      "vo2"                = "Vo₂ max",
      "waistcm"            = "Waist circ (cm)",
      "waist2height"       = "Waist-to-height ratio",
      "bmiz"               = "BMI (z)",
      "bodyfat"            = "Body fat (%)",
      "bpsysamp"           = "Sys BP (amp)",
      "pulsepressamp"      = "Pulse pressure (amp)",
      "bpsysz"             = "Sys BP (z)",
      "bpdiaz"             = "Dia BP (z)",
      "trigly"             = "Triglycerides",
      "cholesttotal"       = "Total cholesterol",
      "cholesttotalhdl"    = "HDL cholesterol",
      "cholestnonhdl"      = "Non-HDL cholesterol",
      "glucose"            = "Glucose",
      .default = outcome  # keeps original value if not matched
    )
  )

  model_predictions$type <- factor(model_predictions$type, levels = c("Not adjusted", "Adjusted"))
  
  fig_plot <- model_predictions |>
        ggplot(aes(x = st_int, y = mean, color = type, group = type)) +
        geom_point() +
        geom_line() +
        geom_blank(aes(y = y_min)) +
        geom_blank(aes(y = y_max)) +
        facet_wrap(~ outcome_recode,scales = "free_y" ) +
        theme_bw() +
        theme(text = element_text(family = "serif"),
              strip.text.y = element_text(angle = 0)) + 
        labs(color = "Model type", x = "Screen Time (z; constant across timepoints)", y = "Health Outcome")
  ggplot2::ggsave(fig_plot,
                width = 35,
                height = 20,
                units = "cm",
                filename = glue::glue("outputs/model_predictions.png"))

}
