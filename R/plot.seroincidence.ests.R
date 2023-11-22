#' Plot `seroincidence.by` log-likelihoods
#'
#' @param x a '"seroincidence.by"' object (from [est.incidence.by()])
#' @inheritDotParams plot.seroincidence
#' @return aan object of class `"ggarrange"`, which is a [ggplot2::ggplot()] or a [list()] of [ggplot2::ggplot()]s.
#' @export
#'
plot.seroincidence.by = function(x, ...)
{

  if(length(x) == 0)
  {
    stop("The input doesn't contain any fits. Did subsetting go wrong?")
  }

  if(!attr(x,"graphs_included"))
  {

    stop(
      "Graphs cannot be extracted; ",
      "`build_graph` was not `TRUE` in the call to `est.incidence.by()`")
    figure = NULL
  }

  labels = names(x)
  figs = lapply(x, FUN = plot.seroincidence, ...)

  for (i in 1:length(figs))
  {
    figs[[i]] = figs[[i]] + ggplot2::ggtitle(labels[i])
  }

  ncol = length(figs) |> sqrt() |> ceiling()
  nrow = ceiling(length(figs)/ncol)
  figure <- do.call(
    what = function(...) ggpubr::ggarrange(
      ...,
      ncol = ncol,
      nrow = nrow),
    args = figs)

  return(figure)

}
