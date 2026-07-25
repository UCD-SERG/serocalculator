#' @title Summarizing fitted seroincidence models
#' @description
#' This function is a `summary()` method for `seroincidence` objects.
#' When the model was fit with clustered data (using the `cluster_var`
#' parameter in [est_seroincidence()]), this function automatically computes
#' cluster-robust standard errors to account for within-cluster correlation.
#'
#' @param object a [list()] outputted by [stats::nlm()] or [est_seroincidence()]
#' @param coverage desired confidence interval coverage probability
#' @param verbose whether to produce verbose messaging
#' @param ... unused
#'
#' @return a [tibble::tibble()] containing the following:
#' * `est.start`: the starting guess for incidence rate
#' * `ageCat`: the age category we are analyzing
#' * `incidence.rate`: the estimated incidence rate, per person year
#' * `SE`: standard error of the incidence rate estimate
#' * `CI.lwr`: lower limit of confidence interval for incidence rate
#' * `CI.upr`: upper limit of confidence interval for incidence rate
#' * `se_type`: type of standard error used ("standard" or "cluster-robust")
#' * `coverage`: coverage probability
#' * `log.lik`:
#'    log-likelihood of the data used in the call to `est_seroincidence()`,
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
#'         or `stepmin` in [est_seroincidence()]
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
#' est1 <- est_seroincidence(
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
  cluster_var <- object |> attr("cluster_var")
  stratum_var <- object |> attr("stratum_var")

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

  # Check if cluster-robust variance should be computed
  use_cluster_robust <- !is.null(cluster_var)

  if (use_cluster_robust) {
    if (verbose) {
      cli::cli_inform(c(
        "i" = paste(
          "Computing cluster-robust standard errors using",
          "{.field {cluster_var}} variable."
        )
      ))
    }
    var_log_lambda <- .compute_cluster_robust_var(
      fit = object,
      cluster_var = cluster_var,
      stratum_var = stratum_var
    )
  } else {
    # Standard variance from Hessian
    var_log_lambda <- 1 / object$hessian |> as.vector()
  }

  # Ensure var_log_lambda is a scalar
  var_log_lambda <- as.numeric(var_log_lambda)[1]
  se_log_lambda <- sqrt(var_log_lambda)

  to_return <- tibble::tibble(
    est.start = start,
    incidence.rate = exp(log_lambda),
    SE = se_log_lambda * .data$incidence.rate, # delta method:
    # https://en.wikipedia.org/wiki/Delta_method#Univariate_delta_method
    CI.lwr = exp(log_lambda - qnorm(1 - h_alpha) * se_log_lambda),
    CI.upr = exp(log_lambda + qnorm(1 - h_alpha) * se_log_lambda),
    se_type = if (use_cluster_robust) "cluster-robust" else "standard",
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
