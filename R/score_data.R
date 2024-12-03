score_data <- function(df_clean) {

  df_clean$ApoBA1_ratio <- df_clean$apolipa1 / df_clean$apolipb
  
  # Diet classificaiton
  df_clean$diet_SSB_flag <- df_clean$servessoftdrink != "I don't drink soft drinks, cordials or sports drinks"
  # 2 or more serves of fruit
  df_clean$diet_fruit_flag <- as.numeric(df_clean$servesfruit) >= 4
  # 5 or more serves of veg per day
  df_clean$diet_veg_flag <- as.numeric(df_clean$servesveg) >= 7


}
