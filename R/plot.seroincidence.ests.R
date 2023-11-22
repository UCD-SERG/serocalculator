#' Plot `seroincidence.ests` log-likelihoods
#'
#' @param x a '"seroincidence.ests"' object (from [est.incidence.by()])
#' @param ... unused
#' @return aan object of class `"ggarrange"`, which is a [ggplot2::ggplot()] or a [list()] of [ggplot2::ggplot()]s.
#' @export
#'
plot.seroincidence.ests = function(x, ...)
{

  if(!attr(x,"graphs_included"))
  {

    stop(
      "Graphs cannot be extracted from the `seroincidence.ests` object.",
      "`build_graph` was not `TRUE` in the call to `est.incidence.by()`")
    figure = NULL
  }

  labels = x |> attr("Strata") |> pull("Stratum")
  figs = lapply(x, FUN = attr, which = "ll_graph")

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
