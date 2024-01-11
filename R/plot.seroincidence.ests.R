#' Plot `seroincidence.by` log-likelihoods
#' @description
#' Plots log-likelihood curves by stratum, for `seroincidence.by` objects
#' @param object a '"seroincidence.by"' object (from [est.incidence.by()])
#' @param ncol number of columns to use for panel of plots
#' @inheritDotParams autoplot.seroincidence
#' @return an object of class `"ggarrange"`, which is a [ggplot2::ggplot()] or a [list()] of [ggplot2::ggplot()]s.
#' @export
#'
autoplot.seroincidence.by = function(
    object,
    ncol = min(3, length(object)),
    ...)
{

  if(length(object) == 0)
  {
    stop("The input doesn't contain any fits. Did subsetting go wrong?")
  }

  if(!attr(object,"graphs_included"))
  {

    stop(
      "Graphs cannot be extracted; ",
      "`build_graph` was not `TRUE` in the call to `est.incidence.by()`")
    figure = NULL
  }

  labels = names(object)
  figs = lapply(object, FUN = autoplot.seroincidence, ...)

  for (i in 1:length(figs))
  {
    figs[[i]] = figs[[i]] + ggplot2::ggtitle(labels[i])
  }


  nrow = ceiling(length(figs)/ncol)
  figure <- do.call(
    what = function(...) ggpubr::ggarrange(
      ...,
      ncol = ncol,
      nrow = nrow),
    args = figs)

  return(figure)

}
