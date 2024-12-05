make_table1 <- function(scored_data) {
  require(gtsummary)
  require(dplyr)
  theme_gtsummary_journal("jama", set_theme = TRUE)
  # get table data
  table_dat <- scored_data %>%
    dplyr::select(
      wave,
      age, sex, indig, ses, st_total, accmvpa, accsed, diet, sexualmaturity, vo2, waistcm,
      waist2height, bmiz, bodyfat, bpsys, bpdia, fastingtime, glucose, trigly, cholesttotal,
      cholesttotalhdl, cholestnonhdl, phospholipids, apolipa1, apolipb, ApoBA1_ratio,
      glycoprotein
    ) %>%
    dplyr::mutate(
      wave = dplyr::case_match(
        wave,
        3 ~ "Wave 3",
        4 ~ "Wave 4",
        5 ~ "Wave 5",
        6 ~ "Wave 6",
        6.5 ~ "Checkpoint"
      ) %>%
        factor(levels = c("Wave 3", "Wave 4", "Wave 5", "Wave 6", "Checkpoint"))
    )

    tbl_summary(table_dat,
      by = wave,
      statistic = list(
        all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ "{n} / {N} ({p}%)"
      ),
      label = list(
        age ~ "Age (Years)",
        sex ~ "Sex",
        indig ~ "Indigenous Status",
        ses ~ "Socioeconomic Status",
        st_total ~ "Aggregated Screen Time (min/w)",
        accmvpa ~ "MVPA (min/day)",
        accsed ~ "Sedentary Time (min/day)",
        diet ~ "Diet",
        sexualmaturity ~ "Sexual Maturity",
        vo2 ~ "VO2 Max",
        waistcm ~ "Waist Circumference (cm)",
        waist2height ~ "Waist-to-Height Ratio",
        bmiz ~ "BMI Z-Score",
        bodyfat ~ "Body Fat (%)",
        bpsys ~ "Systolic Blood Pressure (mmHg)",
        bpdia ~ "Diastolic Blood Pressure (mmHg)",
        fastingtime ~ "Fasting Time (hours)",
        glucose ~ "Glucose (mmol/L)",
        trigly ~ "Triglycerides (mmol/L)",
        cholesttotal ~ "Total Cholesterol (mmol/L)",
        cholesttotalhdl ~ "Cholesterol:HDL Ratio",
        cholestnonhdl ~ "Non-HDL Cholesterol (mmol/L)",
        phospholipids ~ "Phospholipids (mmol/L)",
        apolipa1 ~ "Apolipoprotein A1 (g/L)",
        apolipb ~ "Apolipoprotein B (g/L)",
        ApoBA1_ratio ~ "ApoB:ApoA1 Ratio",
        glycoprotein ~ "Glycoprotein (mmol/L)"
      )
    ) %>%
    modify_spanning_header(all_stat_cols() ~ "**Wave Point**") %>%
    modify_table_body(
      ~ .x %>% dplyr::filter(!grepl("Non-Indigenous", label))
    ) %>%
    modify_table_body(
      ~ .x %>% dplyr::filter(!grepl("Unknown", label))
    ) %>%
    modify_table_body(
      ~ .x %>%
        dplyr::mutate(across(everything(), ~ ifelse(grepl("NA|^0$", .), "-", .)))
    ) %>%
    modify_table_body(
      ~ .x %>%
        dplyr::mutate(label = if_else(label == "does not meet guidelines", "Does Not Meet Guidelines", label))
    ) %>%
    modify_table_body(
      ~ .x %>%
        dplyr::mutate(label = if_else(label == "meets guidelines", "Meets Guidelines", label))
    )
}
