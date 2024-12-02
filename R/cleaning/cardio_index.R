#' Calculate Cardiovascular Index
#'
#' This function calculates the cardiovascular index based on various health metrics.
#'
#' @param age Numeric. Age of the individual.
#' @param gender Character. Gender of the individual ("Male" or "Female").
#' @param SBP Numeric. Systolic Blood Pressure (mmHg).
#' @param DBP Numeric. Diastolic Blood Pressure (mmHg).
#' @param BMI Numeric. Body Mass Index (kg/m2).
#' @param waist_circumference Numeric. Waist Circumference (cm).
#' @param total_body_fat Numeric. Total Body Fat percentage.
#' @param total_cholesterol Numeric. Total Cholesterol (mmol/L).
#' @param LDL_C Numeric. LDL Cholesterol (mmol/L).
#' @param HDL_C Numeric. HDL Cholesterol (mmol/L).
#' @param triglycerides Numeric. Triglycerides (mmol/L).
#' @param glucose Numeric. Glucose (mmol/L).
#' @param bio_ref_data Data frame. Reference data containing mean and standard deviation for each metric.
#'
#' @return Numeric. The calculated cardiovascular index.
#' @export
#'
get_cardio_index <- function(age,
                             gender,
                             SBP,
                             DBP,
                             BMI,
                             waist_circumference,
                             total_body_fat,
                             total_cholesterol,
                             LDL_C,
                             HDL_C,
                             triglycerides,
                             glucose,
                             bio_ref_data){
  
  # Initialize a list to store Z-scores
  z_scores <- list()
  
  # Helper function to calculate Z-score
  calc_z <- function(value, mean, sd) {
    (value - mean) / sd
  }
 
 get_z <- function(val, outcome) {
    if(!is.null(val)){
      mean_i <- bio_ref_data[Variable == "Mean" & Sex == gender & Age_years == age, outcome, with = FALSE]
      sd_i <- bio_ref_data[Variable == "SD" & Sex == gender, outcome, with = FALSE] 
      if(length(mean_i) > 0 && length(sd_i) > 0){
        z <- calc_z(val, mean_i, sd_i)
        z
      }
 }
    else{
      NA
    }
 }

 metrics <- c("SBP (mmHg)", "DBP (mmHg)", "BMI (kg/m2)", "WC (cm)", "TC (mmol/L)", "LDL-C (mmol/L)", "HDL-C (mmol/L)", "TG (mmol/L)", "Glucose (mmol/L)")

 vals <- c(SBP, DBP, BMI, waist_circumference, total_cholesterol, LDL_C, HDL_C, triglycerides, glucose)

cardio_index <- lapply(seq_along(metrics), function(i) get_z(vals[i], metrics[i])) |>
  unlist() |>
  mean()

  cardio_index
} 
