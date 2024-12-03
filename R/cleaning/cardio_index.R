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
                             sex,
                             SBP,
                             DBP,
                             waist_circumference,
                             total_body_fat,
                             HDL_C,
                             triglycerides,
                             glucose,
                             bio_ref_data) {
  # Helper function to calculate Z-score
  calc_z <- function(value, mean, sd) {
    (value - mean) / sd
  }

  inverted_HDL_C <- HDL_C * -1

  get_z <- function(val, outcome) {
    if (!is.null(val)) {
      mean_i <- bio_ref_data[Variable == "Mean" & Sex == sex & Age_years == age, outcome, with = FALSE]
      sd_i <- bio_ref_data[Variable == "SD" & Sex == sex, outcome, with = FALSE]
      if(length(mean_i) > 0 && length(sd_i) > 0) {
        z <- calc_z(val, mean_i, sd_i)
        z
      }
    } else {
      NA
    }
  }
   metrics <- c("SBP (mmHg)", "DBP (mmHg)", "WC (cm)", "HDL-C (mmol/L)", "TG (mmol/L)", "Glucose (mmol/L)")

  vals <- c(SBP, DBP, waist_circumference,
            inverted_HDL_C, triglycerides, glucose)

  cardio_index <- lapply(seq_along(metrics),
                         function(i) get_z(vals[i], metrics[i]))
  names(cardio_index) <- metrics
  cardio_index$BP_combined <- mean(unlist(cardio_index[c("SBP (mmHg)", "DBP (mmHg)")]))
  
  cardio_index[c("SBP (mmHg)", "DBP (mmHg)")] <- NULL
  out <- mean(unlist(cardio_index))
  out
}
