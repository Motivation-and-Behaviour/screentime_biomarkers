scale_variables <- function(data, id_var = "id") {
  # get all numeric variables
  numeric_vars <- dplyr::select(data, where(is.numeric))
  # loop through all numeric variables
  for (i in seq_along(numeric_vars)) {
    var_name <- names(numeric_vars)[i]
    x <- numeric_vars[[i]]
    # don't scale id variables
    if (var_name == id_var) {
      next
    }
    # If all missing nothing to do
    if (all(is.na(x))) next
    scaled_name <- paste0(var_name, "_scaled")
    scaled_x <- scale(x)
    mean_diff_x <- mean(x - scaled_x, na.rm = TRUE)
    # if variable is not close to being standardised
    if (mean_diff_x < 0.5) {
      # add the scaled variable to the data
      data[[scaled_name]] <- scaled_x
    }
  }
  data
}
