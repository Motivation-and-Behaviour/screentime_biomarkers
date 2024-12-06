make_diagnostic_table <- function(
    model_tables,
    outpath = "outputs/model_tables.csv") {
  model_tables$pvalue <- metaKIN::round_p(model_tables$pvalue)
  model_tables$est <- round(model_tables$coef, 2)
  model_tables$ci.lower <- round(model_tables$ci_l, 2)
  model_tables$ci.upper <- round(model_tables$ci_u, 2)
  model_tables$est_95 <- glue::glue(
    "{model_tables$est} [{model_tables$ci.lower}, {model_tables$ci.upper}]"
  )
  model_tables$se <- round(model_tables$se, 2)
  model_tables$z <- round(model_tables$z, 2)

  model_tables[, .(
    Outcome = outcome,
    Term = term,
    Coef = est_95,
    SE = se,
    z = z,
    p = pvalue,
    type = type
  )] |>
    data.table::fwrite(outpath)

  outpath
}
