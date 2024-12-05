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
    checkpoint_only = TRUE, no_outliers = TRUE) {
  waves_joined[waves_joined == -9] <- NA

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
      valid_pa = if_else(accvalidwkdays >= 3 & accvalidwedays >= 1,
        "Valid", "Insufficient"
      ) %>% as.factor(),
      across(c(accmvpa, accsed), ~
        if_else(valid_pa == "Valid", .x, NA_real_)),
      mean_bloodpressure = rowMeans(cbind(bpsys, bpdia), na.rm = TRUE),
    )

  # Identify kids with health conditions
  conditions_ids <-
    dplyr::filter(
      tidy_df, wave == 6.5,
      (condition_vision == "Selected" |
        condition_pa == "Selected" |
        condition_breath == "Selected" |
        condition_feetlegs == "Selected")
    ) %>%
    dplyr::pull(id)

  tidy_df <- tidy_df %>%
    mutate(health_condition = if_else(
      id %in% conditions_ids, "Health condition", "No health condition"
    ) %>% as.factor())

  if (no_outliers) {
    vars_to_remove_outliers <-
      dplyr::select(tidy_df, where(is.numeric), -c(id, wave)) %>% colnames()
    # don't remove outliers from vars starting with screentime variables
    vars_to_remove_outliers <-
      vars_to_remove_outliers[!stringr::str_detect(vars_to_remove_outliers, "^st")]
    # perform outlier removal
    tidy_df[, vars_to_remove_outliers] <- apply_remove_outliers(tidy_df, vars_to_remove_outliers)
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

full_df %>%
  group_by(wave) %>%
  dplyr::summarise(n())

tidy_df %>%
  group_by(wave) %>%
  dplyr::summarise(n())
