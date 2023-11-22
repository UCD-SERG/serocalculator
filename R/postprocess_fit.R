
#' postprocess a fitted incidence model
#'
#' @param fit a [list()], outputted by [stats::nlm()] or [serocalculator::.optNLL()]
#' @param coverage desired confidence interval coverage probability
#' @param start starting value for incidence rate
#'
#' @return a [tibble::tibble()]; see [stats::nlm()] for details on `code` variable
#' @export
#'
postprocess_fit = function(
    fit,
    coverage = .95,
    start = fit |> attr("lambda.start"))
{


  alpha = 1 - coverage
  h.alpha = alpha/2

  log.lambda.est = dplyr::tibble(
    est.start = start,
    incidence.rate = exp(fit$estimate),
    SE = sqrt(1/fit$hessian) |> as.vector(),
    CI.lwr = exp(fit$estimate - qnorm(1 - h.alpha) * .data$SE),
    CI.upr = exp(fit$estimate + qnorm(1 - h.alpha) * .data$SE),
    coverage = coverage,
    log.lik = -fit$minimum,
    iterations = fit$iterations,
    nlm.exit.code = nlm_exit_codes[fit$code]) |>
    structure(
      graph = fit |> attr("ll_graph")
    )

  class(log.lambda.est) =
    "summary.seroincidence.est" |>
    union(class(log.lambda.est))

  return(log.lambda.est)
}
