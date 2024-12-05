make_model_dfs <- function(model, fit) {
  lapply(seq_along(model), function(i) {
    make_lgcm_table_df(model[[i]]) |>
      dplyr::mutate(type = ifelse(i == 1, "unadjusted", "adjusted"))
  }) |>
    dplyr::bind_rows()
}
