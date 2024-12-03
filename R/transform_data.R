transform_data <- function(scored_data) {
  transformed_data <- scored_data |>
    tidyr::pivot_wider(
      names_from = wave, # Create columns based on wave
      values_from = -c(id, wave), # Keep id fixed, spread all other variables
    names_sep = "_w"
  )
    transformed_data
}
