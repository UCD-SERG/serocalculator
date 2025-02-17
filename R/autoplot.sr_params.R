#' graph longitudinal seroresponse parameters by antigen isotype
#'
#'`r lifecycle::badge("deprecated")`
#'
#' `autoplot.curve_params()` was renamed to [autoplot.sr_params()] to use more accurate terminology.
#'
#' @keywords internal
#' @export
autoplot.sr_params <- function(...) {
  lifecycle::deprecate_warn("1.3.0", "autoplot.curve_params()", "autoplot.sr_params()")
  autoplot.sr_params(...)
}

#' @inheritParams plot_sr_params_one_ab
#' @inheritDotParams plot_sr_params_one_ab
#' @param antigen_isos antigen isotypes to analyze (can subset `sr_params`)
#' @param ncol how many columns of subfigures to use in panel plot
#' @details
#' ## `rows_to_graph`
#' If you directly specify `rows_to_graph` when calling this function,
#' the row numbers are enumerated separately for each antigen isotype;
#' in other words, for the purposes of this argument,
#' row numbers start over at 1 for each antigen isotype.
#' There is currently no way to specify different row numbers for different antigen isotypes;
#' if you want to do that,
#' you will could call [plot_sr_params_one_ab()] directly for each antigen isotype
#' and combine the resulting panels yourself.
#' Or you could subset `sr_params` manually,
#' before passing it to this function,
#' and set the `n_curves` argument to `Inf`.
#' @return a [ggplot2::ggplot()] object
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#' library(ggplot2)
#' library(magrittr)
#'
#' sr_params <-
#'   serocalculator_example("example_sr_params.csv") %>%
#'   read.csv() %>%
#'   as_sr_params() %>%
#'   filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) %>%
#'   autoplot()
#'
#' sr_params
#' }
autoplot.sr_params <- function(
    object,
    antigen_isos = unique(object$antigen_iso),
    ncol = min(3, length(antigen_isos)),
    ...) {
  split_data <- object %>%
    filter(.data$antigen_iso %in% antigen_isos) %>%
    droplevels() %>%
    split(~antigen_iso)

  labels <- names(split_data)
  figs <- split_data %>%
    lapply(FUN = plot_sr_params_one_ab, ...)

  for (i in seq_along(figs)) {
    figs[[i]] <- figs[[i]] + ggplot2::ggtitle(labels[i])
  }


  nrow <- ceiling(length(figs) / ncol)
  figure <- do.call(
    what = function(...) {
      ggpubr::ggarrange(
        ...,
        ncol = ncol,
        nrow = nrow
      )
    },
    args = figs
  )

  return(figure)
}
