make_model_predictions <- function(m = model_vo2_w6.5, transformed_data, n = 1000) {
browser()
  require(lavaan)
  future::plan("multisession", workers = 20)
  covariates <- c("female", "indig", "ses_w6", "bad_diet", "sexualmaturity_numeric_w6.5")
  outcome <- attr(m, "outcome")
  boot_model <- sem(m[[1]], data = transformed_data, bootstrap = 1000)
  plot_dat <- lapply(seq_along(m), function(x){
           make_prediction(
                           m[[x]],
                           type = names(m)[x],
                           vals = c(-2,0,2),
                           outcome,
                           covariates,
                           transformed_data,
                            n = n)
              })
  plot_dat <- data.table::rbindlist(plot_dat)
  plot_dat$type <- dplyr::recode(plot_dat$type, "lgcm_fit" = "Not adjusted", "lgcm_adj_fit" = "Adjusted")
  plot_dat
}

make_prediction <- function(m, type, vals = c(-2,0,2), outcome, covariates, transformed_data, n) {

  pred_data <- transformed_data[1:9,]
  st_vars <- grep("^st.*_scaled", colnames(pred_data), value = TRUE)

  # Set all screen time variables to
  pred_data_low <- pred_data
  pred_data_low[, st_vars] <- rep(vals,3)
  pred_data_low[, covariates] <- 0

  out <- future.apply::future_lapply(1:n, function(i){
           sampled_data <- transformed_data[sample(1:nrow(transformed_data), replace = TRUE), ]
           preds <- lavaan::growth(
              m,
              data = sampled_data,
              fixed.x = FALSE,
              baseline = FALSE,
              control = list(iter.max = 25),
              check.gradient = FALSE,
              check.post = FALSE,
              start = "simple",
              h1 = FALSE,
              auto.th = FALSE,
              auto.delta = FALSE,
              se = "none",
              test = "none"
              ) |>
           lavPredictY(
              newdata = pred_data_low,
              ynames = lavNames(m, "ov.y"),
              xnames = c(st_vars, covariates),
           )
           out <- data.frame(outcome = outcome, mean = preds[1:3], type = type, st_vals = vals)
           out
              })

  out <- data.table::rbindlist(out)

  out[, {
    sorted_means <- sort(mean)
    alpha = 0.05
    lower_i <- floor(n * alpha / 2)
    upper_i <- ceiling((1-alpha/2) * n)
    lower_conf <- sorted_means[[lower_i]]
    upper_conf <- sorted_means[[upper_i]]
    .(est = mean(mean), lower = lower_conf, upper = upper_conf)
  }  , by = c("type","st_vals")]

}
