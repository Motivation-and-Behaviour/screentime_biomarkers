#' Remove Statistical Outliers from a Vector
#'
#' This function removes outliers from a numeric vector by replacing values 
#' that are more than 4 standard deviations from the mean with NA.
#'
#' @param x A numeric vector
#'
#' @return A numeric vector with outliers replaced by NA
#'
#' @examples
#' x <- c(1, 2, 3, 100)  # 100 is an outlier
#' remove_outliers(x)
#' 
remove_outliers <- function(x) {
 x[scale(x) > 4] <- NA
 x
}

#' Remove Outliers from Multiple Columns
#'
#' This function applies the remove_outliers function to multiple columns
#' in a data frame.
#'
#' @param data A data frame containing the columns to process
#' @param cols A character vector of column names to process
#'
#' @return A matrix or data frame with outliers replaced by NA in specified columns
#'
#' @examples
#' df <- data.frame(a = c(1, 2, 3, 100), b = c(1, 2, 3, 200))
#' apply_remove_outliers(df, c("a", "b"))
#'
apply_remove_outliers <- function(data, cols) {
  sapply(data[cols], remove_outliers)
}

