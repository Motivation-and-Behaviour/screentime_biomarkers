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
      "bpdiaz"             = "Dia BP",
      "trigly"             = "Triglycerides",
      "cholesttotal"       = "Total cholesterol",
      "cholesttotalhdl"    = "HDL cholesterol",
      "cholestnonhdl"      = "Non-HDL cholesterol",
      "glucose"            = "Glucose",
      .default = outcome  # keeps original value if not matched
    )
  )
  model_predictions$st_slope <- as.factor(model_predictions$st_slope)
browser()
  model_predictions$type <- factor(model_predictions$type, levels = c("Not adjusted", "Adjusted"))
    plot_subpreds <-  function(dat, label) {
      fig_plot <- dat |>
        ggplot(aes(x = st_int, y = mean, color = st_slope, group = st_slope)) +
        geom_point() +
        geom_line() +
        facet_grid(rows = vars(outcome_recode), cols = vars(type), scales = "free_y") +
        theme_bw() +
        theme(text = element_text(family = "serif"),
              strip.text.y = element_text(angle = 0)) + 
        labs(color = "Screentime slope (z)", x = "Screen Time Intercept (z)", y = "Health Outcome")
      ggplot2::ggsave(fig_plot,
                    width = 25,
                    height = 30,
                    units = "cm",
                    filename = glue::glue("outputs/{label}.png"))
  }

  # Define groups
  metabolic_lipid_profiles <- c(
    "apoba1 ratio", "glycoprotein", "phospholipids", 
    "trigly", "cholesttotal", "cholesttotalhdl", 
    "cholestnonhdl", "glucose"
  )

  anthropometric_cardiovascular <- c(
    "cardio index", "vo2", "waistcm", "waist2height", 
    "bmiz", "bodyfat", "bpsysamp", "pulsepressamp", 
    "bpsysz", "bpdiaz"  
  )

# Filter code
  filtered_data_metabolic <- model_predictions |>
    dplyr::filter(outcome %in% metabolic_lipid_profiles)

  filtered_data_anthropometric <- model_predictions |>
    dplyr::filter(outcome %in% anthropometric_cardiovascular)

  # Plot code
  plot_subpreds(filtered_data_metabolic, "metabolic_lipid_profiles")
  plot_subpreds(filtered_data_anthropometric, "anthropometric_cardiovascular")

}
