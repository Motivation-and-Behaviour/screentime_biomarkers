#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param waves_joined df. The survey data.
#' @param biomarkers_data df. The biomarkers data.
#' @param checkpoint_only logical. If TRUE, restrict data to only those present
#' at the checkpoint wave.
#' @param remove_outliers logical. If TRUE, remove outliers from the data.
#' @return
#' @author {Taren Sanders}
#' @export
clean_data <- function(
    waves_joined, biomarkers_data,
    checkpoint_only = TRUE, remove_outliers = TRUE) {
  full_df <-
    dplyr::bind_rows(waves_joined, biomarkers_data)

  tidy_df <-
    full_df %>%
    dplyr::mutate(
      indig = dplyr::case_when(
        stringr::str_detect(indig, "Yes") ~ "Indigenous",
        indig == "No" ~ "Non-Indigenous",
        TRUE ~ NA_character_
      ) %>% as.factor(),
      age = age_weeks / 52.1775, # Number of weeks in a year
      # Make NA those with insufficent wear time
      across(c(accmvpa, accsed), ~
        if_else(accvalidwkdays >= 3 & accvalidwedays >= 1, .x, NA_real_))
    )

  if (remove_outliers) {
    tidy_df <-
      apply_remove_outliers(
        tidy_df,
        dplyr::select(tidy_df, where(is.numeric), -c(id, wave)) %>% colnames()
      ) %>%
      as_tibble()
  }

  if (checkpoint_only) {
    checkpoint_ids <-
      dplyr::filter(full_df, wave == 6.5) %>%
      dplyr::pull(id)

    tidy_df <- tidy_df %>%
      dplyr::filter(id %in% checkpoint_ids)
  }

  tidy_df
}
