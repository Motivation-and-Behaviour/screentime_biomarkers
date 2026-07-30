#' Calculate Cardiovascular Index
#'
#' Calculates a continuous cardiometabolic risk score as the mean of age- and
#' sex-standardised z-scores, following Stavnsbo et al. (2018). Waist
#' circumference and triglycerides are natural-log transformed and standardised
#' against the log-scale reference values; HDL-C is inverted (after
#' standardising) because it is protective; systolic and diastolic blood
#' pressure are averaged into a single combined component.
#'
#' @param age Numeric. Age of the individual (years, integer-matched to the reference data).
#' @param sex Character. Sex of the individual ("Male" or "Female").
#' @param SBP Numeric. Systolic Blood Pressure (mmHg).
#' @param DBP Numeric. Diastolic Blood Pressure (mmHg).
#' @param waist_circumference Numeric. Waist Circumference (cm); log-transformed internally.
#' @param HDL_C Numeric. HDL Cholesterol (mmol/L).
#' @param triglycerides Numeric. Triglycerides (mmol/L); log-transformed internally.
#' @param glucose Numeric. Glucose (mmol/L).
#' @param bio_ref_data Data frame. Reference data containing mean and standard deviation for each metric.
#'
#' @return Numeric. The calculated cardiometabolic risk score.
#' @export
#'
get_cardio_index <- function(age,
                             sex,
                             SBP,
                             DBP,
                             waist_circumference,
                             HDL_C,
                             triglycerides,
                             glucose,
                             bio_ref_data) {
  # Helper function to calculate Z-score
  calc_z <- function(value, mean, sd) {
    (value - mean) / sd
  }

  get_z <- function(val, outcome, i) {
    if (!is.null(val)) {
      # message(i)
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
  # WC and TG log-transformed per Stavnsbo et al. (2018)
  metrics <- c("SBP (mmHg)", "DBP (mmHg)", "WC (log)", "HDL-C (mmol/L)", "TG (log)", "Glucose (mmol/L)")

  vals <- c(SBP, DBP, log(waist_circumference),
            HDL_C, log(triglycerides), glucose)

  cardio_index <- lapply(seq_along(metrics),
                         function(i) get_z(vals[i], metrics[i], i))
  names(cardio_index) <- metrics
  # HDL-C is protective — invert so higher values lower composite risk
  cardio_index[["HDL-C (mmol/L)"]] <- -1 * cardio_index[["HDL-C (mmol/L)"]]
  cardio_index$BP_combined <- mean(unlist(cardio_index[c("SBP (mmHg)", "DBP (mmHg)")]), na.rm = TRUE)
  
  cardio_index[c("SBP (mmHg)", "DBP (mmHg)")] <- NULL
  out <- mean(unlist(cardio_index), na.rm = TRUE)
  out
}
