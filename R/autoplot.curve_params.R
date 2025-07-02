#' Graph antibody decay curves by antigen isotype
#'
#' @param method a [character] string indicating whether to use
#'  - [graph.curve.params()] (default) or
#'  - [graph_seroresponse_model_1()] (previous default)
#'
#' as the graphing method.
#'
#' @details
#' Currently, the backend for this method is [graph.curve.params()].
#' Previously, the backend for this method was [graph_seroresponse_model_1()].
#' That function is still available if preferred.
#'
#'
#' @inheritParams graph.curve.params
#' @inheritDotParams graph.curve.params
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
    method = c("graph.curve.params", "graph_seroresponse_model_1"),
    ...) {

  # spaghettified in order to swap out implementations with minimal
  # disruption to API
  method <- match.arg(method)
  cur_function <- match.fun(method)
  object |>
    cur_function(...)
}
