model_predictions <- function(m = model_vo2_w6.5, transformed_data) {
  require(lavaan)
  require(ggeffects)
  covariates <- c("female", "indig", "ses_w6", "bad_diet", "sexualmaturity_numeric_w6.5")
  m <- m$lgcm_adj_fit
  st_vars <- grep("^st", colnames(pred_data), value = TRUE)
  
  # Set all screen time variables to low
  pred_data_low <- transformed_data
  pred_data_low[, st_vars] <- -2
  pred_data_low[, covariates] <- 0

  predict(m, newdata = pred_data_low) |> head()
}
