save_table <- function(table, filepath) {
  gtsummary::as_gt(table) %>%
    gt::gtsave(filepath)

  filepath
}