transform_data <- function(scored_data, bio_ref_data, filter_valid = TRUE) {
  transformed_data <- scored_data |>
    tidyr::pivot_wider(
      names_from = wave, # Create columns based on wave
      values_from = -c(id, wave), # Keep id fixed, spread all other variables
    names_sep = "_w"
  ) |> data.table()

  if(filter_valid) {
    # When in doubt keep the data (when validity is not provided)
    is_valid_wear <- transformed_data$valid_pa_w6.5 == "Valid" | is.na(transformed_data$valid_pa_w6.5)
    transformed_data <- transformed_data[is_valid_wear]
    ## Remove kids with health conditions
    has_condition <- transformed_data$health_condition_w6.5 == "Health condition"
    transformed_data <- transformed_data[!has_condition]
  }

  # get cardio index
    transformed_data$cardio_index_w6.5 <- sapply(seq_len(nrow(transformed_data)), function(i) {
    get_cardio_index(
      age = transformed_data$age_integer_w6.5[i],
      sex = transformed_data$sex_w6.5[i],
      SBP = transformed_data$bpsys_w6.5[i],
      DBP = transformed_data$bpdia_w6.5[i],
      waist_circumference = transformed_data$waistcm_w6.5[i],
      HDL_C = transformed_data$cholesttotalhdl_w6.5[i],
      triglycerides = transformed_data$trigly_w6.5[i],
      glucose = transformed_data$glucose_w6.5[i],
      bio_ref_data = bio_ref_data
    )
  })
# Only get variables we want.
    transformed_data[,.(
      id,
      age = age_w6.5,
      sex = sex_w6.5,
      indig = indig_w6, # most complete
      diet = diet_w6.5, 
      sexualmaturity_w6.5,
      fastingtime_w6.5,
      st_total_w3,
      st_total_w4,
      st_total_w5,
      st_total_w6,
      st_total_w6.5,
      bpsys_w6.5,
      bpdia_w6.5,
      waistcm_w6.5,
      cholesttotalhdl_w6.5,
      cholesttotal_w6.5,
      cholestnonhdl_w6.5,
      trigly_w6.5,
      glucose_w6.5,
      cardio_index_w6.5,
      ApoBA1_ratio_w6.5,
      vo2_w6.5,
      bodyfat_w6.5,
      accmvpa_w6.5,
      accsed_w6.5,
      waist2height_w6.5,
      bmiz_w6.5,
      glycoprotein_w6.5,
      phospholipids_w6.5,
      ses_w3,
      ses_w4,
      ses_w5,
      ses_w6,
      ses_w6.5,
      health_condition_w6.5,
      valid_pa_w6.5
                        ) ]
    
    if(filter_valid) {
      attr(transformed_data, "has_condition") <- has_condition
      attr(transformed_data, "is_valid_wear") <- is_valid_wear
    }

   transformed_data
}
