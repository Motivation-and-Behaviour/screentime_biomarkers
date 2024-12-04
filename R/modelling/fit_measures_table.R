# get_measures for a single model
fm <- function(i, title) {
  require(lavaan)
  measures <- c("cfi", "tli", "rmsea", "srmr")
  out <- as.list(fitMeasures(i, measures))
  out$model_type <- title
  out
}

#' get_measures for adjusted and non adjusted models
get_measures <- function(m, title) {
  out <- lapply(seq_along(m), function(i) fm(m[[i]], names(m)[i])) |>
    rbindlist()
  out$model_name <- title
  out
}

fit_measures_table <- function(all_models) {
  # get fit measures for all models
  models <- lapply(
    seq_along(all_models),
    function(i) get_measures(all_models[[1]], title = names(all_models)[i])
  )
  fit_dat <- rbindlist(models)

  # Load dplyr package
  require(dplyr)

  # Round numeric columns to 3 decimal places using mutate_if
  fit_dat <- fit_dat |>
    dplyr::mutate_if(is.numeric, round, 2) |>
    dplyr::select(model_name, everything())

  fit_dat
}
