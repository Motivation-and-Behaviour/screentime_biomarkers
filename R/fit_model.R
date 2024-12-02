#' Fit a Linear Mixed-Effects Model with Multiple Optimizers
#'
#' This function fits a linear mixed-effects model using the `lme4` package, 
#' attempting multiple optimizers until convergence is achieved or all 
#' optimizers are exhausted.
#'
#' @param ... Additional arguments passed to `lme4::lmer`
#' @param data A data frame containing the variables in the model
#' @param max_iter Maximum number of iterations for the optimizer
#'
#' @return A fitted `lmerMod` object. If convergence is not achieved, a warning is issued and the 
#'         returned model object will have an attribute `conv` set to FALSE.
#'
#' @examples
#' \dontrun{
#'   data(sleepstudy, package = "lme4")
#'   fit <- fit_model(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#'   summary(fit)
#' }
#'
#' @importFrom lme4 lmer lmerControl
#' @importFrom optimx optimx
#' @importFrom dfoptim dfoptim
#' @importFrom butcher axe_env
#' @export
fit_model <- function(..., data, max_iter = 1e5) {
  require(optimx)
  require(lme4)
  require(dfoptim)

  conv <- FALSE
  exhausted <- FALSE
  i <- 1
  meth.tab <- lme4:::meth.tab.0
  meth.tab <- cbind(meth.tab, maxit_name = c("maxfun", "maxfun", "maxit", "maxfeval", "maxit", "maxeval", "maxeval"))
  meth.tab <- meth.tab[sample(seq_len(nrow(meth.tab)), nrow(meth.tab), replace = FALSE), ]

  while (!conv & i <= nrow(meth.tab)) {
    if (meth.tab[i, 2] != "") {
      optCtrl <- list(method = unname(meth.tab[i, 2]))
    } else {
      optCtrl <- list()
    }

    optCtrl[[meth.tab[i, 3]]] <- max_iter

    mod <- lme4::lmer(
      ...,
      data = data,
      control = lmerControl(
        optimizer = meth.tab[i, 1],
        optCtrl = optCtrl
      )
    )
    mod@call$control$optimizer <- unname(meth.tab[i, 1])
    mod@call$control$optCtrl <- unlist(optCtrl)

    if (is_conv(mod)) {
      conv <- TRUE
      attr(mod, "conv") <- TRUE
      return(mod)
    }
    i <- i + 1
  }
  warning("No convergence with any optimizer:", ...)
  attr(mod, "conv") <- FALSE
  mod@call$formula <- butcher::axe_env(mod@call$formula)
  mod
}
