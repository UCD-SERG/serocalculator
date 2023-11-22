#' Plot the log-likelihood curve for the incidence rate estimate
#'
#' @param x `seroincidence` object (from [est.incidence()])
#' @param ... unused
#'
#' @return a [ggplot2::ggplot()]
#' @export
#'
plot.seroincidence = function(x, ...)
{
  to_return = attr(x, "ll_graph")

  if(is.null(to_return))
  {
    warning(
      "Graphs cannot be extracted from the `seroincidence` object.",
      "`build_graph` was not `TRUE` in the call to `est.incidence()`")
    figure = NULL
  }

  return(to_return)
}
