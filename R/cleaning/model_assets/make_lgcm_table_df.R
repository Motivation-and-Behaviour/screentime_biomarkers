make_lgcm_table_df <- function(fit) {
  require(lavaan)
  est <- parameterEstimates(fit) |>
    as.data.table()
  tabby <- est[op == "~"]
  tabby[lhs != "st_intercept", .(
    outcome = lhs,
    term = rhs,
    coef = est,
    ci_l = ci.lower,
    ci_u = ci.upper,
    se = se,
    z = z,
    pvalue = pvalue
  )]
}
