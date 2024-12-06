make_model_predictions <- function(m = model_vo2_w6.5, transformed_data) {
  require(lavaan)
  covariates <- c("female", "indig", "ses_w6", "bad_diet", "sexualmaturity_numeric_w6.5")
  outcome <- attr(m, "outcome")
  boot_model <- sem(m[[1]], data = transformed_data, bootstrap = 1000)
  plot_dat <- lapply(seq_along(m), function(x){
           make_prediction(
                           m[[x]],
                           type = names(m)[x],
                           int_vals = c(-2,0,2),
                           outcome,
                           covariates,
                           transformed_data)
              })
  plot_dat <- data.table::rbindlist(plot_dat)
  plot_dat$type <- dplyr::recode(plot_dat$type, "lgcm_fit" = "Not adjusted", "lgcm_adj_fit" = "Adjusted")
  plot_dat
}

make_prediction <- function(m, type, int_vals = c(-2,0,2), outcome, covariates, transformed_data) {

  st_vars <- grep("^st.*_scaled", colnames(transformed_data), value = TRUE) |>
    sort()
  # Set all screen time variables to
  param_grid <- expand.grid(int = int_vals)
  pred_data_fin <- lapply(seq_len(nrow(param_grid)), function(i) {
    pred_data_n <- data.frame(transformed_data[1,])
    pred_data_n[, st_vars] <- param_grid[i, "int"]
    pred_data_n
    }) |> data.table::rbindlist() |>
  data.frame()
  
  for(i in seq_along(covariates)) {
    # set all covariates to their respective means from the main data
    pred_data_fin[,covariates[i]] <- mean(transformed_data[[covariates[i]]], na.rm = TRUE)
    }
  preds <- lavPredictY(
    m,
    newdata = rbindlist(list(pred_data_fin, transformed_data)), # pred funciton assumes a large dataset
    ynames = lavNames(m, "ov.y"),
    xnames = c(st_vars, covariates),
    )
   preds <- preds[seq_len(nrow(pred_data_fin))]
# record +1 and -1 SD for outcome
  sd_outcome <- sd(transformed_data[[outcome]], na.rm = TRUE)
  mean_outcome <- mean(transformed_data[[outcome]], na.rm = TRUE)
  data.frame(outcome = outcome,
           mean = preds,
           type = type, st_int = param_grid[,"int"],
           y_min = mean_outcome - sd_outcome,
           y_max = mean_outcome + sd_outcome
           )
}
