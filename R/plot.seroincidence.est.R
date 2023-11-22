#' Plot the log-likelihood curve for the incidence rate estimate
#'
#' @param x `seroincidence.est` object (from [find.MLE()])
#' @param ... unused
#'
#' @return a [ggplot2::ggplot()]
#' @export
#'
plot.seroincidence.est = function(x, ...)
{
  to_return = attr(x, "ll_graph")

  if(is.null(to_return))
  {
    warning(
      "Graphs cannot be extracted from the `seroincidence.est` object.",
      "`build_graph` was not `TRUE` in the call to `find.MLE()`")
    figure = NULL
  }

  return(to_return)
}
