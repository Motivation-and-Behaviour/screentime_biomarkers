library(lavaan)
tar_load(transformed_data)

tar_load(model_vo2_w6.5)


data <- transformed_data
fit <- model_vo2_w6.5$lgcm_adj_fit

factor_scores <- lavPredict(fit)
factor_scores_df <- as.data.frame(factor_scores)
rownames(factor_scores_df)
# Check the names of the latent variables
colnames(factor_scores)

data <- tibble(st_intercept = factor_scores[, "st_intercept"], st_slope = factor_scores[, "st_slope"])

parameter_estimates <- parameterEstimates(fit)

# Get coefficients for the regression of the outcome on intercept and slope
beta_intercept <- parameter_estimates$est[parameter_estimates$lhs == "vo2_w6.5" & parameter_estimates$rhs == "st_intercept"]
beta_slope <- parameter_estimates$est[parameter_estimates$lhs == "vo2_w6.5" & parameter_estimates$rhs == "st_slope"]
beta_covariates <- parameter_estimates$est[parameter_estimates$lhs == "vo2_w6.5" & parameter_estimates$op == "~" & !(parameter_estimates$rhs %in% c("st_intercept", "st_slope"))]



intercept_range <- seq(from = quantile(data$st_intercept, 0.10), to = quantile(data$st_intercept, 0.90), length.out = 100)
slope_range <- seq(from = quantile(data$st_slope, 0.10), to = quantile(data$st_slope, 0.90), length.out = 100)

# Create a grid of intercept and slope values
grid_data <- expand.grid(
  st_intercept = intercept_range,
  st_slope = slope_range
)
