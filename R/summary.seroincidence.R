#' @title Summarizing fitted seroincidence models
#' @description
#' This function is a `summary()` method for `seroincidence` objects.
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
#' * `CI.lwr`: lower limit of confidence interval for incidence rate
#' * `CI.upr`: upper limit of confidence interval for incidence rate
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
#' * `measurement.noise.<antigen>`, `measurement.noise.<antigen>`, etc.:
#'   measurement noise parameters (eps) for each antigen isotype, where
#'   `<antigen>` is the antigen-isotype name
#' * `biological.noise.<antigen>`, `biological.noise.<antigen>`, etc.:
#'   biological noise parameters (nu) for each antigen isotype, where
#'   `<antigen>` is the antigen-isotype name
#' * `n.seroresponse.params`: number of longitudinal seroresponse parameter
#'   observations
#' * `n.pop.data`: number of population data observations
#' * `seroresponse.params.stratified`: logical indicating whether seroresponse
#'   parameters were stratified (`FALSE` for unstratified)
#' * `seroresponse.params.name`: name of the seroresponse parameters object
#' * `noise.params.name`: name of the noise parameters object
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
  noise_params <- object |> attr("noise_params")
  n_sr_params <- object |> attr("n_sr_params")
  n_pop_data <- object |> attr("n_pop_data")
  sr_params_stratified <- object |> attr("sr_params_stratified")
  sr_params_name <- object |> attr("sr_params_name")
  noise_params_name <- object |> attr("noise_params_name")

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

  # Add noise parameters as columns with antigen_iso names
  if (!is.null(noise_params) && nrow(noise_params) > 0) {
    for (i in seq_len(nrow(noise_params))) {
      antigen_name <- make.names(noise_params$antigen_iso[i])
      col_name_eps <- paste0("measurement.noise.", antigen_name)
      col_name_nu <- paste0("biological.noise.", antigen_name)
      to_return[[col_name_eps]] <- noise_params$eps[i]
      to_return[[col_name_nu]] <- noise_params$nu[i]
    }
  }

  # Add metadata counts and object names as columns
  to_return$n.seroresponse.params <- n_sr_params
  to_return$n.pop.data <- n_pop_data
  to_return$seroresponse.params.stratified <- sr_params_stratified
  to_return$seroresponse.params.name <- sr_params_name
  to_return$noise.params.name <- noise_params_name

  class(to_return) <-
    "summary.seroincidence" |>
    union(class(to_return))

  return(to_return)
}
