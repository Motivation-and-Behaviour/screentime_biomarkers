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
  out
}
