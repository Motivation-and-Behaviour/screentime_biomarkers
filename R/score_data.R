score_data <- function(df_clean) {
  df_clean$ApoBA1_ratio <- df_clean$apolipb / df_clean$apolipa1

  # Diet classificaiton
  df_clean$diet_SSB_flag <- df_clean$servessoftdrink != "I don't drink soft drinks, cordials or sports drinks"
  # 3 or more serves of fruit
  df_clean$diet_fruit_flag <- as.numeric(df_clean$servesfruit) >= 5
  # 4 or more serves of veg per day.
  df_clean$diet_veg_flag <- as.numeric(df_clean$servesveg) >= 6
  df_clean$diet <- ifelse(df_clean$diet_SSB_flag + df_clean$diet_fruit_flag + df_clean$diet_veg_flag == 3,
    "meets guidelines", "does not meet guidelines"
  ) |>
    factor(levels = c("does not meet guidelines", "meets guidelines"))

  # Cardio score variable
  df_clean$age_integer <- as.integer(round(df_clean$age, 0))
  # Estimate st_total and remove outliers
  df_clean$st_total <- df_clean$st_comp_minweek + df_clean$st_vg_minweek + df_clean$st_tv_minweek
  df_clean$st_total <- remove_outliers(df_clean$st_total)
  df_clean$st_total <- df_clean$st_total / 7 # use min/day
  # return df_clean

  # Handle factors with  factors cannot have more than 2 levels
  df_clean$sexualmaturity_numeric <- as.numeric(gsub("Tanner", "", df_clean$sexualmaturity))
  df_clean$indig <- as.numeric(df_clean$indig == "Indigenous")
  df_clean$female <- as.numeric(df_clean$sex == "Female")
  df_clean$bad_diet <- as.numeric(df_clean$diet == "does not meet guidelines")
  df_clean
}
