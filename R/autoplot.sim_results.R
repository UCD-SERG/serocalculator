#' Plot simulation results
#' `autoplot()` method for `sim_results` objects
#'
#' @param object a `sim_results` object (from [analyze_sims()])
#' @param statistic which column of `object` should be the y-axis?
#' @param ... unused
#' @returns a [ggplot2::ggplot]
#' @export
#'
#' @example inst/examples/exm-autoplot.sim_results.R
autoplot.sim_results <- function(
    object,
    statistic = "Empirical_SE",
    ...)
{
  object |>
    dplyr::mutate(lambda.sim = factor(lambda.sim)) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data$sample_size,
        group = .data$lambda.sim,
        col = .data$lambda.sim,
        y = .data[[statistic]]) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme(legend.position = "bottom")
}
