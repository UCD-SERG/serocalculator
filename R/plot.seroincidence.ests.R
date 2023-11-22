plot.seroincidence.ests = function(object)
{

  if(!attr(object,"graphs_included"))
  {

    stop(
      "Graphs cannot be extracted from the `seroincidence.ests` object.",
      "`build_graph` was not `TRUE` in the call to `est.incidence.by()`")
    figure = NULL
  }

  requireNamespace("ggpubr", quietly = FALSE)
  labels = sapply(object, FUN = attr, which = "stratum_string")
  figs = lapply(object, FUN = attr, which = "ll_graph")
  figure <- ggpubr::ggarrange(
    figs,
    labels = c("A", "B", "C"),
    ncol = length(figs) |> sqrt() |> ceiling(),
    nrow = length(figs) |> sqrt() |> ceiling())


}
