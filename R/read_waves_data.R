#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param filepath
read_waves_data <- function(filepath) {
  wave_num <- dplyr::case_when(
    stringr::str_detect(filepath, "4\\.sav") ~ 3,
    stringr::str_detect(filepath, "6\\.sav") ~ 4,
    stringr::str_detect(filepath, "8\\.sav") ~ 5,
    stringr::str_detect(filepath, "10\\.sav") ~ 6,
    TRUE ~ NA_real_
  )
  wave_letter <-
    dplyr::case_match(
      wave_num,
      3 ~ "c",
      4 ~ "d",
      5 ~ "e",
      6 ~ "f"
    )

  comp_var_name <- dplyr::if_else(wave_num < 5, "comweek", "othweek")

  raw_data <-
    foreign::read.spss(filepath, to.data.frame = TRUE, use.value.labels = TRUE)

  raw_data %>%
    dplyr::transmute(
      id = hicid,
      wave = wave,
      # Screen time
      st_comp_minweek = .data[[glue::glue("{wave_letter}{comp_var_name}")]],
      st_vg_minweek = .data[[glue::glue("{wave_letter}egweek")]],
      st_tv_minweek = .data[[glue::glue("{wave_letter}tvweek")]],
      # Covariates
      age_weeks = .data[[glue::glue("{wave_letter}scagew")]],
      sex = zf02m1,
      indig = zf12m1,
      ses = .data[[glue::glue("{wave_letter}sep2")]]
    ) %>%
    dplyr::as_tibble()
}
