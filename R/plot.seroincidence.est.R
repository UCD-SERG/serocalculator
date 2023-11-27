#' Plot the log-likelihood curve for the incidence rate estimate
#'
#' @param x a `seroincidence` object (from [est.incidence()])
#' @param log_x should the x-axis be on a logarithmic scale (`TRUE`) or linear scale (`FALSE`, default)?
#' @param ... unused
#'
#' @return a [ggplot2::ggplot()]
#' @export
#'
plot.seroincidence = function(x, log_x = FALSE, ...)
{
  to_return = attr(x, "ll_graph")

  if(is.null(to_return))
  {
    stop(
      "Graphs cannot be extracted; ",
      "`build_graph` was not `TRUE` in the call to `est.incidence()`")
    figure = NULL
  }

  if(log_x)
  {
    to_return = to_return +
      ggplot2::scale_x_continuous(
        trans = "log10",
        labels = scales::label_comma())
  }

  return(to_return)
}
