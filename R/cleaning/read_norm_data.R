read_norm_data <- function(filepath) {
  dt <- data.table::fread(filepath)
  dt
}
