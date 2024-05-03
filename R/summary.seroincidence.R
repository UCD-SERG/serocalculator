
#' @title Summarizing fitted seroincidence models
#' @description This function is a `summary()` method for `seroincidence` objects.
#' @param object a [list()], outputted by [stats::nlm()] or [est.incidence()]
#' @param coverage desired confidence interval coverage probability
#' @param ... unused
#' @return a [tibble::tibble()] containing the following:
#' * `est.start`: the starting guess for incidence rate
#' * `ageCat`: the age category we are analyzing
#' * `incidence.rate`: the estimated incidence rate, per person year
#' * `CI.lwr`: lower limit of confidence interval for incidence rate
#' * `CI.upr`: upper limit of confidence interval for incidence rate
#' * `coverage`: coverage probability
#' * `log.lik`: log-likelihood of the data used in the call to `est.incidence()`, evaluated at the maximum-likelihood estimate of lambda (i.e., at `incidence.rate`)
#' * `iterations`: the number of iterations used
#'  * `antigen_isos`: a list of antigen isotypes used in the analysis
#'  * `nlm.convergence.code`: information about convergence of the likelihood maximization procedure performed by `nlm()` (see "Value" section of [stats::nlm()], component `code`); codes 3-5 indicate issues:
#'    * 1: relative gradient is close to zero, current iterate is probably solution.
#'    * 2: successive iterates within tolerance, current iterate is probably solution.
#'    * 3: Last global step failed to locate a point lower than x. Either x is an approximate local minimum of the function, the function is too non-linear for this algorithm, or `stepmin` in [est.incidence()] (a.k.a., `steptol` in [stats::nlm()]) is too large.
#'    * 4: iteration limit exceeded; increase `iterlim`.
#'    * 5: maximum step size `stepmax` exceeded five consecutive times. Either the function is unbounded below, becomes asymptotic to a finite value from above in some direction or `stepmax` is too small.
#' @export
#' @examples
#' library(dplyr)
#'
#' xs_data <- load_pop_data(file_path = "https://osf.io/download//n6cp3/",
#'                          age = "Age",
#'                          id = "index_id",
#'                          value = "result",
#'                          standardize = TRUE)
#'
#' curves = load_curve_params("https://osf.io/download/rtw5k/")
#' noise = load_noise_params("https://osf.io/download//hqy4v/")
#'
#' est1 = est.incidence(
#' pop_data = xs_data %>% filter(Country == "Pakistan"),
#' curve_params = curves,
#' noise_params = noise %>% filter(Country == "Pakistan"),
#' antigen_isos = c("HlyE_IgG", "HlyE_IgA")
#' )
#'
#' summary(est1)

summary.seroincidence = function(
    object,
    coverage = .95,
    ...)
{
  start = object %>% attr("lambda_start")
  antigen_isos = object %>% attr("antigen_isos")

  alpha = 1 - coverage
  h.alpha = alpha/2
  hessian = object$hessian
  if(hessian < 0)
    warning(
      "`nlm()` produced a negative hessian; something is wrong with the numerical derivatives.",
      "\nThe standard error of the incidence rate estimate cannot be calculated.")

  log.lambda = object$estimate
  var.log.lambda = 1/object$hessian %>% as.vector()
  se.log.lambda = sqrt(var.log.lambda)

  to_return = tibble::tibble(
    est.start = start,
    incidence.rate = exp(log.lambda),
    SE = se.log.lambda * .data$incidence.rate, # delta method:
    # https://en.wikipedia.org/wiki/Delta_method#Univariate_delta_method
    CI.lwr = exp(log.lambda - qnorm(1 - h.alpha) * se.log.lambda),
    CI.upr = exp(log.lambda + qnorm(1 - h.alpha) * se.log.lambda),
    coverage = coverage,
    log.lik = -object$minimum,
    iterations = object$iterations,
    antigen.isos = antigen_isos %>% paste(collapse = "+"),
    nlm.convergence.code = object$code %>% factor(levels = 1:5, ordered = TRUE)
    #%>% factor(levels = 1:5, labels = nlm_exit_codes)
    )

  class(to_return) =
    "summary.seroincidence" %>%
    union(class(to_return))

  return(to_return)
}
