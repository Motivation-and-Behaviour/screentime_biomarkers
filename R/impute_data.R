make_data_imp <- function(df_clean, n_imps = 3) {
  require(mice)

  imp_data <- df_clean

  # Empty imputation to change defaults:
  m0 <- mice(imp_data, maxit = 0)

  # Don't do imputation based on these vars:
  dont_imp <- c("filter", "checkpoint_ids")
  dont_use <- c(
    dont_imp, c()
  )
  # Don't imp some vars, and disable some as predictors
  meth <- m0$method
  pred <- m0$predictorMatrix
  meth[names(meth) %in% dont_imp] <- ""
  pred[, colnames(pred) %in% c(dont_use, dont_imp)] <- 0

  participant_invar <- c("age")

  participant_continuous <-
    c(
      "pa_volume",
      "pa_intensity",
      "pa_intensity_m16",
      "pa_mostactivehr",
      "sleep_duration",
      "sleep_efficiency",
      "sleep_onset",
      "sleep_wakeup",
      "sleep_regularity",
      "sleep_efficiency_lag",
      "sleep_onset_lag",
      "sleep_wakeup_lag",
      "sleep_regularity_lag",
      "sleep_duration_lag"
    )

 # 0 means don't use as predictor
 # 1 means use as predictor
 # -2 means impute as a cluster variable

  # Multi-level imputation, consider correlations within participant
  # These lines were given to change how sex was being predicted
  # pred["sex", ] <- 0
  # pred["sex", c("age", "bmi", "pa_intensity", "screen_time", "sleep_regularity")] <- 1
  pred[c(participant_continuous, participant_invar, "sex"), "id"] <- -2L
  meth[c(participant_continuous)] <- "2l.pmm"
  meth[c(participant_invar)] <- "2lonly.pmm"
  meth["sex"] <- "2lonly.pmm"

  # Run imps with better settings
  future_cores <- min(parallel::detectCores() - 1, n_imps, 16)

  dist_core <- cut(
    1:n_imps, future_cores,
    labels = paste0("core", 1:future_cores)
  )
  n_imp_core <- as.vector(table(dist_core))

  future::plan("multisession",
    workers = future_cores
  )

  imps <- furrr::future_map(n_imp_core, function(x) {
    mice(
      data = imp_data,
      m = x,
      predictorMatrix = pred,
      method = meth,
      printFlag = FALSE,
      seed = NA
    )
  },
  .options = furrr::furrr_options(seed = TRUE, packages = c("mice", "miceadds"))
  )

  future::plan(future::sequential)

  # postprocess clustered imputation into a mids object
  imp <- imps[[1]]
  if (length(imps) > 1) {
    for (i in 2:length(imps)) {
      imp <- ibind(imp, imps[[i]])
    }
  }
  # let imputation matrix correspond to grand m
  for (i in seq_along(imp$imp)) {
    colnames(imp$imp[[i]]) <- 1:imp$m
  }

  # include_scale_variables
  variables_to_scale <-
    c(
    )

  scale_names <- paste0("scale_", variables_to_scale)

  imp_list <- data.table(complete(imp, action = "long", include = TRUE))

  for (v in seq_along(variables_to_scale)) {
    imp_list[, (eval(scale_names[v])) :=
      as.numeric(scale(eval(parse(text = variables_to_scale[v])))), by = ".imp"]
  }

  as.mids(imp_list)
}
