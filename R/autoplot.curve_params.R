#' Graph antibody decay curves by antigen isotype
#' @details
#' Currently, the backend for this method is [graph.curve.params()].
#' Previously, the backend for this method was [graph_seroresponse_model_1()].
#' That function is still available if preferred.
#'
#'
#' @inheritDotParams graph.curve.params
#' @param antigen_isos antigen isotypes to analyze
#' (can subset `curve_params`)
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
    ...) {

  # spaghettified in order to swap out implementations with minimal
  # disruption to API
  object |>
    graph.curve.params(...)
}
