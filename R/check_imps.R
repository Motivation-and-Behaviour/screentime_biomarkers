#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_imp
#' @return
#' @author Taren Sanders
#' @export
check_imps <- function(data_imp) {
  require(dplyr)
  require(ggplot2)

  imps <- as_tibble(mice::complete(data_imp, action = "long", include = TRUE))

  # Invariant variables should be the same within participants in each imp
  participant_invar <- c("age", "weight", "height", "bmi")
  stopifnot(
    "An invariant variable varies within a particpant" =
      all(sapply(participant_invar, function(x) {
        n_distinct(select(imps, .imp, participant_id)) ==
          n_distinct(select(imps, .imp, participant_id, {{ x }}))
      }))
  )

  # Some variables should always be positive
  positive_vars <- c(
    "pa_volume",
    "sleep_duration",
    "sleep_efficiency",
    "sleep_onset",
    "sleep_wakeup",
    "sleep_efficiency_lag",
    "sleep_onset_lag",
    "sleep_wakeup_lag",
    "sleep_duration_lag",
    "age",
    "weight",
    "height",
    "bmi",
    "daylight_hours",
    "pa_mostactivehr"
  )

  stopifnot(
    "A postive variable contains negative numbers" =
      all(sapply(positive_vars, function(x) {
        nrow(filter(imps, .imp > 0 & !!sym(x) < 0)) == 0
      }))
  )

  # Make some density plots
  imps_long <- imps %>%
    select(
      ".imp", ".id", "pa_volume", "pa_intensity", "pa_intensity_m16",
      "sleep_duration", "sleep_efficiency", "sleep_onset", "sleep_wakeup",
      "sleep_regularity", "sleep_efficiency_lag", "sleep_onset_lag",
      "sleep_wakeup_lag", "sleep_regularity_lag", "sleep_duration_lag",
      "age", "weight", "height", "bmi", "daylight_hours", "pa_mostactivehr"
    ) %>%
    reshape2::melt(c(".imp", ".id")) %>%
    mutate(
      imputed = if_else(.imp == 0, "Observed", "Imputed"),
      value = as.numeric(value)
    )

  plot <- ggplot(imps_long, aes(x = value, group = .imp, colour = imputed)) +
    stat_density(
      geom = "path", position = "identity",
      alpha = 0.4, linewidth = 0.5
    ) +
    facet_wrap(~variable, ncol = 4, scales = "free")

  filename <- "Figures/explore/imps_density.png"
  ggsave(filename, plot, width = 12, height = 12, dpi = 300)

  return(filename)
}
