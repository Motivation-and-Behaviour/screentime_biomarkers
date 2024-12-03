score_data <- function(df_clean, bio_ref_data) {

  df_clean$ApoBA1_ratio <- df_clean$apolipa1 / df_clean$apolipb
  
  # Diet classificaiton
  df_clean$diet_SSB_flag <- df_clean$servessoftdrink != "I don't drink soft drinks, cordials or sports drinks"
  # 2 or more serves of fruit
  df_clean$diet_fruit_flag <- as.numeric(df_clean$servesfruit) >= 4
  # 5 or more serves of veg per day. Only 133 observations do!
  df_clean$diet_veg_flag <- as.numeric(df_clean$servesveg) >= 7
  df_clean$diet <- ifelse(df_clean$diet_SSB_flag + df_clean$diet_fruit_flag + df_clean$diet_veg_flag == 3, "meets guidelines", "does not meet guidelines")

}
