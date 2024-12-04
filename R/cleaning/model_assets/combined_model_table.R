get_model_table <- function(m) {
  lapply(seq_along(m), function(i) {
    make_lgcm_table_df(m[[i]]) |>
      dplyr::mutate(type = ifelse(i == 1, "unadjusted", "adjusted"))
  }) |>
    dplyr::bind_rows()
}
