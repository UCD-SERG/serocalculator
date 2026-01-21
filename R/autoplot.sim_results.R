#' Plot simulation results
#' `autoplot()` method for `sim_results` objects
#'
#' @param object a [data.frame]
#' containing the columns expected
#' for a `sim_results` object (from [analyze_sims()])
#' @param statistic which column of `object` should be the y-axis
#' @param x_var [character]: which column in `object` to use for the x-axis
#' @param ... unused
#' @param group_var [character]: which column in `object` to use for the
#' `group` aesthetic in [ggplot2::aes()]
#' @param color_var [character]: which column in `object` to use for the
#' `color` aesthetic in [ggplot2::aes()]
#'
#' @returns a [ggplot2::ggplot]
#' @export
#'
#' @example inst/examples/exm-autoplot.sim_results.R
autoplot.sim_results <- function(
    object,
    statistic = "Empirical_SE",
    x_var = "sample_size",
    group_var = "lambda.sim",
    color_var = group_var,
    ...) {
  object |>
    dplyr::mutate(lambda.sim = factor(.data$lambda.sim)) |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = .data[[x_var]],
      group = .data[[group_var]],
      col = .data[[color_var]],
      y = .data[[statistic]]
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme(legend.position = "bottom")
}
