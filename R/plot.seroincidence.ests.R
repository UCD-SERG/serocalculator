#' Plot `seroincidence.ests` log-likelihoods
#'
#' @param object a '"seroincidence.ests"' object (from [est.incidence.by()])
#'
#' @return a [ggplot2::ggplot()] object
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
