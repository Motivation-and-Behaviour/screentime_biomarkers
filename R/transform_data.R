transform_data <- function(scored_data) {
  scored_data |>
    dplyr::mutate_all(as.character) |>
    tidyr::pivot_wider(
      names_from = wave, # Create columns based on wave
      values_from = -c(id, wave), # Keep id fixed, spread all other variables
    names_sep = "_w"
  )
}
