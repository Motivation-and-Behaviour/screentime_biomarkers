#' Summarise the skewness of candidate outcomes
#'
#' Computes the marginal (moment-based) skewness of each candidate outcome on the
#' analysis sample, used to decide which standalone outcomes are re-fit on the
#' natural-log scale in the log-transformed-outcome sensitivity analysis
#' (Sensitivity analysis 4). An outcome is flagged when it is at least moderately
#' right-skewed (skewness above `threshold`) and strictly positive (a
#' requirement for the log transform).
#'
#' @param transformed_data data.table. The analysis data (filtered sample).
#' @param outcomes character. Candidate outcome column names.
#' @param threshold numeric. Skewness cut-off for flagging (default 0.8).
#'
#' @return data.table with one row per outcome (n, min, median, max, skewness,
#'   and the `log_flag` decision), ordered by descending skewness.
#' @export
make_outcome_skewness <- function(transformed_data,
                                  outcomes,
                                  threshold = 0.8) {
  skewness <- function(x) {
    x <- x[is.finite(x)]
    m <- mean(x)
    s <- sqrt(mean((x - m)^2))
    mean(((x - m) / s)^3)
  }

  res <- data.table::data.table(
    outcome = outcomes,
    n = vapply(outcomes, function(v) sum(is.finite(transformed_data[[v]])), integer(1)),
    min = vapply(outcomes, function(v) min(transformed_data[[v]], na.rm = TRUE), numeric(1)),
    median = vapply(outcomes, function(v) stats::median(transformed_data[[v]], na.rm = TRUE), numeric(1)),
    max = vapply(outcomes, function(v) max(transformed_data[[v]], na.rm = TRUE), numeric(1)),
    skewness = vapply(outcomes, function(v) skewness(transformed_data[[v]]), numeric(1))
  )
  res[, log_flag := skewness > threshold & min > 0]
  res[order(-skewness)]
}
