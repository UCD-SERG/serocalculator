#' @title Summarizing fitted seroincidence models
#' @description
#' This function is a `summary()` method for `seroincidence` objects.
#'
#' @param object a [list()], outputted by [stats::nlm()] or [estimate_scr()]
#' @param coverage desired confidence interval coverage probability
#' @param verbose whether to produce verbose messaging
#' @param ... unused
#'
#' @return a [tibble::tibble()] containing the following:
#' * `est.start`: the starting guess for incidence rate
#' * `ageCat`: the age category we are analyzing
#' * `incidence.rate`: the estimated incidence rate, per person year
#' * `CI.lwr`: lower limit of confidence interval for incidence rate
#' * `CI.upr`: upper limit of confidence interval for incidence rate
#' * `coverage`: coverage probability
#' * `log.lik`:
#'    log-likelihood of the data used in the call to `estimate_scr()`,
#'    evaluated at the maximum-likelihood estimate of lambda
#'    (i.e., at `incidence.rate`)
#' * `iterations`: the number of iterations used
#'  * `antigen_isos`: a list of antigen isotypes used in the analysis
#'  * `nlm.convergence.code`:
#'    information about convergence of the likelihood maximization procedure
#'    performed by `nlm()`
#'    (see "Value" section of [stats::nlm()], component `code`);
#'    codes 3-5 indicate issues:
#'    * 1: relative gradient is close to zero,
#'         current iterate is probably solution.
#'    * 2: successive iterates within tolerance,
#'         current iterate is probably solution.
#'    * 3: Last global step failed to locate a point lower than x.
#'         Either x is an approximate local minimum of the function,
#'         the function is too non-linear for this algorithm,
#'         or `stepmin` in [estimate_scr()]
#'         (a.k.a., `steptol` in [stats::nlm()]) is too large.
#'    * 4: iteration limit exceeded; increase `iterlim`.
#'    * 5: maximum step size `stepmax` exceeded five consecutive times.
#'         Either the function is unbounded below,
#'        becomes asymptotic to a finite value from above in some direction,
#'        or `stepmax` is too small.
#' @export
#' @examples
#'
#' library(dplyr)
#'
#' xs_data <-
#'   sees_pop_data_pk_100
#'
#' curve <-
#'   typhoid_curves_nostrat_100 |>
#'   filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))
#'
#' noise <-
#'   example_noise_params_pk
#'
#' est1 <- estimate_scr(
#'   pop_data = xs_data,
#'   sr_params = curve,
#'   noise_params = noise,
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA")
#' )
#'
#' summary(est1)
summary.seroincidence <- function(
    object,
    coverage = .95,
    verbose = TRUE,
    ...) {
  start <- object |> attr("lambda_start")
  antigen_isos <- object |> attr("antigen_isos")

  alpha <- 1 - coverage
  h_alpha <- alpha / 2
  hessian <- object$hessian
  if (verbose && hessian < 0) {
    cli::cli_warn(
      "{.fun nlm} produced a negative hessian;
      something is wrong with the numerical derivatives.",
      "\nThe standard error of the incidence rate estimate
      cannot be calculated."
    )
  }

  log_lambda <- object$estimate
  var_log_lambda <- 1 / object$hessian |> as.vector()
  se_log_lambda <- sqrt(var_log_lambda)

  to_return <- tibble::tibble(
    est.start = start,
    incidence.rate = exp(log_lambda),
    SE = se_log_lambda * .data$incidence.rate, # delta method:
    # https://en.wikipedia.org/wiki/Delta_method#Univariate_delta_method
    CI.lwr = exp(log_lambda - qnorm(1 - h_alpha) * se_log_lambda),
    CI.upr = exp(log_lambda + qnorm(1 - h_alpha) * se_log_lambda),
    coverage = coverage,
    log.lik = -object$minimum,
    iterations = object$iterations,
    antigen.isos = antigen_isos |> paste(collapse = "+"),
    nlm.convergence.code =
      object$code |>
      factor(levels = 1:5, ordered = TRUE) |>
      labelled::set_label_attribute("`nlm()` convergence code")
    # |> factor(levels = 1:5, labels = nlm_exit_codes)
  )

  class(to_return) <-
    "summary.seroincidence" |>
    union(class(to_return))

  return(to_return)
}
