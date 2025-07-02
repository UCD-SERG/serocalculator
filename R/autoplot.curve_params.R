#' Graph antibody decay curves by antigen isotype

#' @param object
#' a `curve_params` object (constructed using [as_curve_params()]), which is
#' a [data.frame()] containing MCMC samples of antibody decay curve parameters
#' @param method a [character] string indicating whether to use
#'  - [graph_seroresponse_model_1()] (default)
#'  - [graph.curve.params()]
#'
#' as the graphing method.
#'
#' @param ... additional arguments passed to the sub-function
#' indicated by the `method` argument.
#' @return a [ggplot2::ggplot()] object
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#' library(ggplot2)
#' library(magrittr)
#'
#' curve <-
#'   serocalculator_example("example_curve_params.csv") |>
#'   read.csv() |>
#'   as_sr_params() |>
#'   filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) |>
#'   autoplot()
#'
#' curve
#' }
autoplot.curve_params <- function(
    object,
    method = c("graph_seroresponse_model_1", "graph.curve.params"),
    ...) {

  # spaghettified in order to swap out implementations with minimal
  # disruption to API
  method <- match.arg(method)
  cur_function <- match.fun(method)
  object |> cur_function(...)
}
