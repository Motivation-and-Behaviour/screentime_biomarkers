find_all_missing_vars <- function(data) {
missing_vars <- sapply(data, function(col) all(is.na(col)))
missing_vars <- names(missing_vars[missing_vars])

if (length(missing_vars) < 0) {
  message("No columns with all NAs found.")
}
missing_vars
}

