#' graph antibody decay curves by antigen isotype
#'
#' @inheritParams graph.decay.curves
#' @inheritDotParams graph.decay.curves
#' @param antigen_isos antigen isotypes to analyze (can be used to subset `curve_params`)
#' @param ncol how many columns of subfigures to use in panel plot
#' @details
#' ## `rows_to_graph`
#' Note that if you directly specify `rows_to_graph` when calling this function, the row numbers are enumerated separately for each antigen isotype; in other words, for the purposes of this argument, row numbers start over at 1 for each antigen isotype. There is currently no way to specify different row numbers for different antigen isotypes; if you want to do that, you will could call [graph.decay.curves()] directly for each antigen isotype and combine the resulting panels yourself. Or you could subset `curve_params` manually, before passing it to this function, and set the `n_curves` argument to `Inf`.
#' @return a [ggplot2::ggplot()] object
#' @export
graph.decay.curves.by = function(
    curve_params,
    antigen_isos = curve_params$antigen_iso |> unique(),
    ncol = min(3, length(antigen_isos)),
    ...)
{

  split_data = curve_params |>
    filter(.data$antigen_iso %in% antigen_isos) |>
    droplevels() |>
    split(~antigen_iso)

  labels = names(split_data)
  figs = lapply(split_data, FUN = graph.decay.curve, ...)

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
