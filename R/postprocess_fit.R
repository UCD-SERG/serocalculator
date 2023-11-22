
#' summarize a fitted incidence model
#'
#' @param object a [list()], outputted by [stats::nlm()] or [serocalculator::est.incidence()]
#' @param coverage desired confidence interval coverage probability
#' @param ... unused
#' @return a [tibble::tibble()]; see [stats::nlm()] for details on `code` variable
#' @export
#'
summary.seroincidence.est = function(
    object,
    coverage = .95,
    ...)
{
  start = object |> attr("lambda.start")
  antigen_isos = object |> attr("antigen_isos")

  alpha = 1 - coverage
  h.alpha = alpha/2

  log.lambda.est = dplyr::tibble(
    est.start = start,
    incidence.rate = exp(object$estimate),
    SE = sqrt(1/object$hessian) |> as.vector(),
    CI.lwr = exp(object$estimate - qnorm(1 - h.alpha) * .data$SE),
    CI.upr = exp(object$estimate + qnorm(1 - h.alpha) * .data$SE),
    coverage = coverage,
    log.lik = -object$minimum,
    iterations = object$iterations,
    antigen.isos = antigen_isos |> paste(collapse = "+"),
    nlm.exit.code = nlm_exit_codes[object$code]) |>
    structure(
      graph = object |> attr("ll_graph")
    )

  class(log.lambda.est) =
    "summary.seroincidence.est" |>
    union(class(log.lambda.est))

  return(log.lambda.est)
}
