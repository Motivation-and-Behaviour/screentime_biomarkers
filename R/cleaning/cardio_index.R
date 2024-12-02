#' Calculate Cardiovascular Index
#'
get_cardio_index <- function(age = 11,
                             gender = "Female",
                             SBP = 114,
                             DBP = 75,
                             BMI = 20,
                             waist_circumference = 70,
                             total_body_fat = 25,
                             total_cholesterol = 4.5,
                             LDL_C = 2.5,
                             HDL_C = 1.3,
                             triglycerides = 1.2,
                             glucose = 5.0,
                             apoA1 = 1.5,
                             apoB = 0.8,
                             bio_ref_data){
  
  # Initialize a list to store Z-scores
  z_scores <- list()
  
  # Helper function to calculate Z-score
  calc_z <- function(value, mean, sd) {
    (value - mean) / sd
  }
  
  # Systolic Blood Pressure
  if (!is.null(SBP)) {
    SP_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, `SBP (mmHg)`]
    SP_sd <- bio_ref_data[Variable == "SD" & Age_years == age & Sex == gender,  `SBP (mmHg)`]
    if(length(SP_mean) > 0 && length(SP_sd) > 0){
      SP_z <- calc_z(SBP, SP_mean, SP_sd)
      z_scores$SBP_z <- SP_z
    }
  }
  
  # Diastolic Blood Pressure
  if (!is.null(DBP)) {
    DBP_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender,`DBP (mmHg)`]
    DBP_sd <- bio_ref_data[Variable == "SD" & Age_years == age & Sex == gender,`DBP (mmHg)`]
    if(length(DBP_mean) > 0 && length(DBP_sd) > 0){
      DBP_z <- calc_z(DBP, DBP_mean, DBP_sd)
      z_scores$DBP_z <- DBP_z
    }
  }
  
  # BMI
  if (!is.null(BMI)) {
    BMI_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, "BMI"]
    BMI_sd <- bio_ref_data[Variable == "SD" & Age_years == age & Sex == gender, "BMI"]
    if(length(BMI_mean) > 0 && length(BMI_sd) > 0){
      BMI_z <- calc_z(BMI, BMI_mean, BMI_sd)
      z_scores$BMI_z <- BMI_z
    }
  }
  
  # Waist Circumference
  if (!is.null(waist_circumference)) {
    WC_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, "Waist_Circumference"]
    WC_sd <- bio_ref_data[Variable == "SD" & Age_years == age & Sex == gender, "Waist_Circumference"]
    if(length(WC_mean) > 0 && length(WC_sd) > 0){
      WC_z <- calc_z(waist_circumference, WC_mean, WC_sd)
      z_scores$WaistCircumference_z <- WC_z
    }
  }
  
  # Total Body Fat Percentage
  if (!is.null(total_body_fat)) {
    BF_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, "Total_Body_Fat"]
    BF_sd <- bio_ref_data[Variable == "SD" & Age_years == age & Sex == gender, "Total_Body_Fat"]
    if(length(BF_mean) > 0 && length(BF_sd) > 0){
      BF_z <- calc_z(total_body_fat, BF_mean, BF_sd)
      z_scores$TotalBodyFat_z <- BF_z
    }
  }
  
  # Total Cholesterol
  if (!is.null(total_cholesterol)) {
    TC_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, "Total_Cholesterol"]
    TC_sd <- bio_ref_data[Variable == "SD" & Age_years == age & Sex == gender, "Total_Cholesterol"]
    if(length(TC_mean) > 0 && length(TC_sd) > 0){
      TC_z <- calc_z(total_cholesterol, TC_mean, TC_sd)
      z_scores$TotalCholesterol_z <- TC_z
    }
  }
  
  # LDL Cholesterol
  if (!is.null(LDL_C)) {
    LDL_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, "LDL_C"]
    LDL_sd <- bio_ref_data[Variable == "SD" & Age_years == age & Sex == gender, "LDL_C"]
    if(length(LDL_mean) > 0 && length(LDL_sd) > 0){
      LDL_z <- calc_z(LDL_C, LDL_mean, LDL_sd)
      z_scores$LDL_C_z <- LDL_z
    }
  }
  
  # HDL Cholesterol
  if (!is.null(HDL_C)) {
    HDL_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, "HDL_C"]
    HDL_sd <- bio_ref_data[Variable == "SD" & Age_years == age & Sex == gender, "HDL_C"]
    if(length(HDL_mean) > 0 && length(HDL_sd) > 0){
      HDL_z <- calc_z(HDL_C, HDL_mean, HDL_sd)
      z_scores$HDL_C_z <- HDL_z
    }
  }
  
  # Non-HDL Cholesterol (calculated if not provided)
  if (is.null(non_HDL_C) && !is.null(total_cholesterol) && !is.null(HDL_C)) {
    non_HDL_C <- total_cholesterol - HDL_C
  }
  
  if (!is.null(non_HDL_C)) {
    nonHDL_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, "Non_HDL_C"]
    nonHDL_sd <- bio_ref_data[Variable == "SD" & Age_years == age & Sex == gender, "Non_HDL_C"]
    if(length(nonHDL_mean) > 0 && length(nonHDL_sd) > 0){
      nonHDL_z <- calc_z(non_HDL_C, nonHDL_mean, nonHDL_sd)
      z_scores$Non_HDL_C_z <- nonHDL_z
    }
  }
  
  # Triglycerides
  if (!is.null(triglycerides)) {
    TG_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, "Triglycerides"]
    TG_sd <- bio_ref_data[Variable == "SD" & Age_years == age & Sex == gender, "Triglycerides"]
    if(length(TG_mean) > 0 && length(TG_sd) > 0){
      TG_z <- calc_z(triglycerides, TG_mean, TG_sd)
      z_scores$Triglycerides_z <- TG_z
    }
  }
  
  # Glucose
  if (!is.null(glucose)) {
    Glu_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, "Glucose"]
    Glu_sd <- bio_ref_data[Variable == "SD" & Age_years == age & Sex == gender, "Glucose"]
    if(length(Glu_mean) > 0 && length(Glu_sd) > 0){
      Glu_z <- calc_z(glucose, Glu_mean, Glu_sd)
      z_scores$Glucose_z <- Glu_z
    }
  }
  
  # Apolipoprotein A1
  if (!is.null(apoA1)) {
    ApoA1_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, "Apolipoprotein_A1"]
    ApoA1_sd <- bio_ref_data[Variable == "SD" & Age_years == age & Sex == gender, "Apolipoprotein_A1"]
    if(length(ApoA1_mean) > 0 && length(ApoA1_sd) > 0){
      ApoA1_z <- calc_z(apoA1, ApoA1_mean, ApoA1_sd)
      z_scores$ApoA1_z <- ApoA1_z
    }
  }
  
  # Apolipoprotein B
  if (!is.null(apoB)) {
    ApoB_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, "Apolipoprotein_B"]
    ApoB_sd <- bio_ref_data[Variable == "SD" & Age_years == age & Sex == gender, "Apolipoprotein_B"]
    if(length(ApoB_mean) > 0 && length(ApoB_sd) > 0){
      ApoB_z <- calc_z(apoB, ApoB_mean, ApoB_sd)
      z_scores$ApoB_z <- ApoB_z
    }
  }
  
  # Calculate the Cardiovascular Index as the mean of Z-scores
  if(length(z_scores) == 0){
    warning("No valid variables provided for Cardiovascular Index calculation.")
    return(NA)
  }
  
  cardio_index <- mean(unlist(z_scores), na.rm = TRUE)
  
  cardio_index
} 
