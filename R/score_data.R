score_data <- function(df_clean, bio_ref_data) {
  df_clean$ApoBA1_ratio <- df_clean$apolipa1 / df_clean$apolipb

  # Diet classificaiton
  df_clean$diet_SSB_flag <- df_clean$servessoftdrink != "I don't drink soft drinks, cordials or sports drinks"
  # 2 or more serves of fruit
  df_clean$diet_fruit_flag <- as.numeric(df_clean$servesfruit) >= 4
  # 5 or more serves of veg per day. Only 133 observations do!
  df_clean$diet_veg_flag <- as.numeric(df_clean$servesveg) >= 7
  df_clean$diet <- ifelse(df_clean$diet_SSB_flag + df_clean$diet_fruit_flag + df_clean$diet_veg_flag == 3, "meets guidelines", "does not meet guidelines")

  # Cardio score variable
  df_clean$age_integer <- as.integer(round(df_clean$age,0))
  df_clean$sex

  df_clean$cardio_index <- future.apply::future_sapply(seq_len(nrow(df_clean)), function(i) {
    get_cardio_index(
      age = df_clean$age_integer[i],
      sex = df_clean$sex[i],
      SBP = df_clean$bpsys[i],
      DBP = df_clean$bpdia[i],
      waist_circumference = df_clean$waistcm[i],
      total_body_fat = df_clean$bodyfat[i],
      HDL_C = df_clean$cholesttotalhdl[i],
      triglycerides = df_clean$trigly[i],
      glucose = df_clean$glucose[i],
      bio_ref_data = bio_ref_data
    )
  })

  df_clean$st_total <- df_clean$st_comp_minweek + df_clean$st_vg_minweek + df_clean$st_tv_minweek
  df_clean
}
