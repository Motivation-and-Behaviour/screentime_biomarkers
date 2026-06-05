#' Plot mean screen time (min/day, 95% CI) across waves.
#' Dashed line marks the Wave 4/5 boundary where the computer/other item
#' was reworded; used to assess definition-change sensitivity (sensitivity 3).
#'
#' @param transformed_data data.table. The wide analysis data (one row per id),
#'   containing `st_total_w3..w6` and `st_consistent_w3..w6` (minutes/day).
#' @return A `ggplot` object.
#' @author Taren Sanders
#' @export
plot_st_wave_trajectory <- function(transformed_data) {
  require(ggplot2)

  measures <- list(
    "Total (all components)" = c(
      "st_total_w3", "st_total_w4", "st_total_w5", "st_total_w6"
    ),
    "Television + electronic games" = c(
      "st_consistent_w3", "st_consistent_w4", "st_consistent_w5", "st_consistent_w6"
    )
  )

  summary_df <- lapply(names(measures), function(label) {
    cols <- measures[[label]]
    lapply(seq_along(cols), function(i) {
      x <- transformed_data[[cols[i]]]
      x <- x[!is.na(x)]
      n <- length(x)
      m <- mean(x)
      se <- stats::sd(x) / sqrt(n)
      data.frame(
        measure = label,
        wave = i + 2L, # columns are Waves 3-6
        mean = m,
        lower = m - stats::qnorm(0.975) * se,
        upper = m + stats::qnorm(0.975) * se
      )
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows()

  summary_df$measure <- factor(
    summary_df$measure,
    levels = c("Total (all components)", "Television + electronic games")
  )

  ggplot(summary_df, aes(
    x = wave, y = mean,
    colour = measure, shape = measure, group = measure
  )) +
    geom_vline(xintercept = 4.5, linetype = "dashed", colour = "grey50") +
    annotate(
      "text",
      x = 4.5, y = max(summary_df$upper),
      label = "Computer/other item reworded",
      hjust = -0.02, vjust = 1, size = 3, family = "serif", colour = "grey40"
    ) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.08) +
    geom_line() +
    geom_point(size = 2) +
    scale_x_continuous(
      breaks = 3:6,
      labels = c("Wave 3", "Wave 4", "Wave 5", "Wave 6")
    ) +
    theme_bw() +
    theme(
      text = element_text(family = "serif"),
      legend.position = "bottom"
    ) +
    labs(
      x = NULL,
      y = "Mean screen time (minutes/day)",
      colour = "Measure",
      shape = "Measure"
    )
}
