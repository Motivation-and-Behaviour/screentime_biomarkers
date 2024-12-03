transform_data <- function(scored_data, bio_ref_data) {
  transformed_data <- scored_data |>
    tidyr::pivot_wider(
      names_from = wave, # Create columns based on wave
      values_from = -c(id, wave), # Keep id fixed, spread all other variables
    names_sep = "_w"
  )
    transformed_data$cardio_index <- sapply(seq_len(nrow(transformed_data)), function(i) {
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


    transformed_data
}
