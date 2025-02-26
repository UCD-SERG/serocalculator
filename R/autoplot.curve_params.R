#' graph antibody decay curves by antigen isotype
#'
#' @inheritParams graph_seroresponse_model_1
#' @inheritDotParams graph_seroresponse_model_1
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
#'   as_curve_params() |>
#'   filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) |>
#'   autoplot()
#'
#' curve
#' }
autoplot.curve_params <- function(
    object,
    antigen_isos = unique(object$antigen_iso),
    ...) {

  # spaghettified in order to swap out implementations with minimal
  # disruption to API
  object |>
    graph_seroresponse_model_1(
      antigen_isos = antigen_isos,
      ...
    )
}
