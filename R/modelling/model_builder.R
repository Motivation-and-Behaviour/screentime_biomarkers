model_builder <-
  function(data_imp,
           outcome,
           predictors,
           control_vars = c(),
           table_only = TRUE,
           ranef,
           terms) {
    require(ata.table)

    formula <-
      glue::glue(
        "{outcome} ~ {paste(predictors, collapse = ' + ')} + {paste(control_vars, collapse = ' + ')} + {ranef}"
      )

    formula <- gsub("\\+  \\+", "+", formula)

    imp_list <- mice::complete(data_imp, "all")

    m <- lapply(imp_list, function(x) {
      mod <- fit_model(formula = eval(parse(text = formula)), data = x)
      mod
    })

    conv <- sapply(m, function(x) is_conv(x))
    conv_p <- sum(conv) / length(conv)

    m_pooled <- mice::pool(m)
    pool_summary <- data.table(summary(m_pooled))

    crit.val <- qnorm(1 - 0.05 / 2)
    pool_summary$lower <-
      papaja::print_num(with(pool_summary, estimate - crit.val * std.error))
    pool_summary$upper <-
      papaja::print_num(with(pool_summary, estimate + crit.val * std.error))

    tabby <- data.table(pool_summary)[
      ,
      list(
        term = term,
        "b [95\\% CI]" = with(
          pool_summary,
          glue::glue("{papaja::print_num(estimate)} [{lower}, {upper}]")
        ),
        se = papaja::print_num(std.error),
        t = papaja::print_num(statistic),
        p = papaja::print_p(p.value)
      )
    ]
    note <- "All models converged." # Add blank note

    if (conv_p < .75) {
      conv_print <- papaja::print_num(conv_p * 100)
      tabby$`b [95\\% CI]` <- paste0(tabby$`b [95\\% CI]`, "$^\\dagger$")
      note <- as.character(glue::glue("$^\\dagger$ these values were derived from a pooled model where fewer than {conv_print}% of models had converged."))
    }

    tabby <- tabby[!grepl("studyid", term), ]

    if (table_only) {
      return(tabby)
    }

    if(moderator == "age"){
      predictor_term <- terms[1]
      predictor_term <- gsub("\\[.*","",predictor_term)
      predictor_term <- paste0(predictor_term,"[-5:5 by=0.1]")
      pred_mat <- get_effects(m,
                  moderator = "age",
                  terms = c(predictor_term, "age[10:80 by = 1]"),
                  outcome = outcome,
                  conv = conv_p,
                  RQ = RQ)
    }else{
      pred_mat = NULL
    }

    # Model_assets
    model_assets <- list(effects = get_effects(
      m,
      moderator = moderator,
      terms = terms,
      outcome = outcome,
      conv = conv_p
    ),
    conv = conv_p,
    diagnostics = check_model(m, conv = conv_p),
    pred_matrix = pred_mat)

    list(
      model_assets = model_assets,
      pooled_model = m_pooled,
      table = tabby,
      note = note,
      control_vars = control_vars
    )
  }
