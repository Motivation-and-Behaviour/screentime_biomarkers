combined_model_table <- function(all_models) {

  adj_tables <- lapply(all_models, function(m){
    m$lgcm_adj_fit |>
      make_lgcm_table_df()
})


  m[[2]] |> make_lgcm_table_df()

}
