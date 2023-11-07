
#' postprocess a fitted incidence model
#'
#' @param fit output from [stats::nlm()]
#' @param coverage desired confidence interval coverage probability
#' @param start starting value for incidence rate
#'
#' @return a [tibble::tibble()]; see [stats::nlm()] for details on `code` variable
#' @export
#'
postprocess_fit = function(
    fit,
    coverage = .95,
    start = NA)
{


  alpha = 1 - coverage
  h.alpha = alpha/2

  log.lambda.est = dplyr::tibble(
    startingval = start,
    incidence.rate = exp(fit$estimate),
    SE = sqrt(1/fit$hessian) |> as.vector(),
    CI.lwr = exp(fit$estimate - qnorm(1 - h.alpha) * SE),
    CI.upr = exp(fit$estimate + qnorm(1 - h.alpha) * SE),
    coverage = coverage,
    log.lik = -fit$minimum,
    iterations = fit$iterations,
    nlm.exit.code = fit$code

    )
}
