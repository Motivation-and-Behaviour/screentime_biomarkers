read_norm_data <- function(filepath = "sources/biomarker_reference.xlsx") {
  dt <- readxl::read_xlsx(filepath) |>
    data.table()
}
