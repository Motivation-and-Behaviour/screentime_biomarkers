make_lgcm_table_df <- function(fit) {
  require(lavaan)
  est <- parameterEstimates(fit) |>
    as.data.table()
  tabby <- est[op == "~"]
  tabby$pvalue <- metaKIN::round_p(tabby$pvalue)
  tabby$est <- round(tabby$est, 2)
  tabby$ci.lower <- round(tabby$ci.lower, 2)
  tabby$ci.upper <- round(tabby$ci.upper, 2)
  tabby$est_95 <- glue::glue("{tabby$est} [{tabby$ci.lower}, {tabby$ci.upper}]")
  tabby$se <- round(tabby$se, 2)
  tabby$z <- round(tabby$z, 2)
  # assemble final table
  tabby[, .(
            Outcome = lhs,
            Term = rhs,
            Coef = est_95,
            SE = se,
            z = z,
            p = pvalue
            )]
}

