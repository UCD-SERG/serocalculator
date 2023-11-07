
#' postprocess a fitted incidence model
#'
#' @param fit output from [stats::nlm()]
#' @param coverage desired confidence interval coverage probability
#' @param start starting value for incidence rate
#'
#' @return a [tibble::tibble()]
#' @export
#'
postprocess_fit = function(
    fit,
    coverage,
    start)
{


  alpha = 1 - coverage
  halpha = alpha/2

  log.lambda.est = dplyr::tibble(
    startingval = start,
    incidence.rate = exp(fit$estimate),
    SE = sqrt(1/fit$hessian),
    CI.lwr = exp(fit$estimate + qnorm(halpha)*SE),
    CI.upr = exp(fit$estimate + qnorm(halpha)*SE),
    coverage = coverage,
    neg.llik = fit$minimum,
    iterations = fit$iterations)
}
