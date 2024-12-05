make_model_predictions <- function(m = model_vo2_w6.5, transformed_data) {
  require(lavaan)
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
                           transformed_data)
              })
  plot_dat <- data.table::rbindlist(plot_dat)
  plot_dat$type <- dplyr::recode(plot_dat$type, "lgcm_fit" = "Not adjusted", "lgcm_adj_fit" = "Adjusted")
  plot_dat
}

make_prediction <- function(m, type, vals = c(-2,0,2), outcome, covariates, transformed_data) {

  pred_data <- transformed_data[1:9,]
  st_vars <- grep("^st.*_scaled", colnames(pred_data), value = TRUE)

  # Set all screen time variables to
  pred_data_low <- pred_data
  pred_data_low[, st_vars] <- rep(vals,3)
  pred_data_low[, covariates] <- 0
browser()
  preds <- lavPredictY(m,
    newdata = pred_data_low,
    ynames = lavNames(m, "ov.y"),
    xnames = c(st_vars, covariates),
    )

data.frame(outcome = outcome, mean = preds[1:3], type = type, st_vals = vals)
}
