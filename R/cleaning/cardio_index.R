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
    SP_sd <- bio_ref_data[Variable == "SD" & Sex == gender,  `SBP (mmHg)`]
    if(length(SP_mean) > 0 && length(SP_sd) > 0){
      SP_z <- calc_z(SBP, SP_mean, SP_sd)
      z_scores$SBP_z <- SP_z
    }
  }
  
  # Diastolic Blood Pressure
  if (!is.null(DBP)) {
    DBP_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender,`DBP (mmHg)`]
    DBP_sd <- bio_ref_data[Variable == "SD" & Sex == gender,`DBP (mmHg)`]
    if(length(DBP_mean) > 0 && length(DBP_sd) > 0){
      DBP_z <- calc_z(DBP, DBP_mean, DBP_sd)
      z_scores$DBP_z <- DBP_z
    }
  }
  
  # BMI
  if (!is.null(BMI)) {
    BMI_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, `BMI (kg/m2)`]
    BMI_sd <- bio_ref_data[Variable == "SD" & Sex == gender, `BMI (kg/m2)`]
    if(length(BMI_mean) > 0 && length(BMI_sd) > 0){
      BMI_z <- calc_z(BMI, BMI_mean, BMI_sd)
      z_scores$BMI_z <- BMI_z
    }
  }
  
  # Waist Circumference
  if (!is.null(waist_circumference)) {
    WC_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, `WC (cm)`]
    WC_sd <- bio_ref_data[Variable == "SD" & Sex == gender, `WC (cm)`]
    if(length(WC_mean) > 0 && length(WC_sd) > 0){
      WC_z <- calc_z(waist_circumference, WC_mean, WC_sd)
      z_scores$WaistCircumference_z <- WC_z
    }
  }
  
  
  # Total Cholesterol
  if (!is.null(total_cholesterol)) {
    TC_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, `TC (mmol/L)`]
    TC_sd <- bio_ref_data[Variable == "SD" & Sex == gender, `TC (mmol/L)`]
    if(length(TC_mean) > 0 && length(TC_sd) > 0){
      TC_z <- calc_z(total_cholesterol, TC_mean, TC_sd)
      z_scores$TotalCholesterol_z <- TC_z
    }
  }
  
  # LDL Cholesterol
  if (!is.null(LDL_C)) {
    LDL_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, `LDL-C (mmol/L)`]
    LDL_sd <- bio_ref_data[Variable == "SD" & Sex == gender, `LDL-C (mmol/L)`]
    if(length(LDL_mean) > 0 && length(LDL_sd) > 0){
      LDL_z <- calc_z(LDL_C, LDL_mean, LDL_sd)
      z_scores$LDL_C_z <- LDL_z
    }
  }
  
  # HDL Cholesterol
  if (!is.null(HDL_C)) {
    HDL_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, `HDL-C (mmol/L)`]
    HDL_sd <- bio_ref_data[Variable == "SD" & Sex == gender, `HDL-C (mmol/L)`]
    if(length(HDL_mean) > 0 && length(HDL_sd) > 0){
      HDL_z <- calc_z(HDL_C, HDL_mean, HDL_sd)
      z_scores$HDL_C_z <- HDL_z
    }
  }
  
  # Triglycerides
  if (!is.null(triglycerides)) {
    TG_mean <- bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, `TG (mmol/L)`]
    TG_sd <- bio_ref_data[Variable == "SD" & Sex == gender, `TG (mmol/L)`]
    if(length(TG_mean) > 0 && length(TG_sd) > 0){
      TG_z <- calc_z(triglycerides, TG_mean, TG_sd)
      z_scores$Triglycerides_z <- TG_z
    }
  }
  
  # Glucose
  if (!is.null(glucose)) {
    Glu_mean <- 
      bio_ref_data[Variable == "Mean" & Age_years == age & Sex == gender, `Glucose (mmol/L)`]
    Glu_sd <- bio_ref_data[Variable == "SD" & Sex == gender, `Glucose (mmol/L)`]
    if(length(Glu_mean) > 0 && length(Glu_sd) > 0){
      Glu_z <- calc_z(glucose, Glu_mean, Glu_sd)
      z_scores$Glucose_z <- Glu_z
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
