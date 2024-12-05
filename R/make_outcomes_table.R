make_outcomes_table <- function(...) {
  list(...)
}

theme_gtsummary_journal("jama", set_theme = TRUE)
tbl_stack(outcomes_table)

class(outcomes_table[[1]])

names(outcomes_table)

unlist(outcomes_table)
