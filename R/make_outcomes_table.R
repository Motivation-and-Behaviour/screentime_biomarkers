make_outcomes_table <- function(...) {
  theme_gtsummary_journal("jama", set_theme = TRUE)
  
  model_tables  <- list(...)
  tbl_stack(model_tables)
}
