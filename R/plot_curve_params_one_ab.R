#' @title Graph an antibody decay curve model
#'
#' @param object a [data.frame()] of curve parameters (one or more MCMC samples)
#' @param verbose verbose output
#' @param xlim range of x values to graph
#' @param n_curves how many curves to plot (see details).
#' @param n_points Number of points to interpolate along the x axis
#' (passed to [ggplot2::geom_function()])
#' @param iters_to_graph which MCMC iterations in `curve_params` to plot
#' (overrides `n_curves`).
#' @param alpha (passed to [ggplot2::geom_function()])
#' how transparent the curves should be:
#' * 0 = fully transparent (invisible)
#' * 1 = fully opaque
#' @param log_x should the x-axis be on a logarithmic scale (`TRUE`)
#' or linear scale (`FALSE`, default)?
#' @param log_y should the Y-axis be on a logarithmic scale
#' (default, `TRUE`) or linear scale (`FALSE`)?
#' @inheritParams ggplot2::geom_function
#' @inheritDotParams ggplot2::geom_function
#' @returns a [ggplot2::ggplot()] object
#' @details
#' ## `n_curves` and `iters_to_graph`
#' In most cases, `object` will contain too many rows of MCMC
#' samples for all of these samples to be plotted at once.
#' * Setting the  `n_curves` argument to a value smaller than the
#' number of rows in `curve_params` will cause this function to select
#' the first `n_curves` rows to graph.
#' * Setting `n_curves` larger than the number of rows in ` will
#' result all curves being plotted.
#' * If the user directly specifies the `iters_to_graph` argument,
#' then `n_curves` has no effect.
#' @examples
#' \donttest{
#' library(dplyr) # loads the `%>%` operator and `dplyr::filter()`
#'
#' curve <-
#'   typhoid_curves_nostrat_100 %>%
#'   filter(antigen_iso == ("HlyE_IgG")) %>%
#'   serocalculator:::plot_curve_params_one_ab()
#'
#'   curve
#' }
#' @keywords internal
plot_curve_params_one_ab <- function(
    object,
    verbose = FALSE,
    alpha = .4,
    n_curves = 100,
    n_points = 1000,
    log_x = FALSE,
    log_y = TRUE,
    iters_to_graph = seq_len(min(n_curves, nrow(object))),
    xlim = c(10 ^ -1, 10 ^ 3.1),
    ...) {
  plot1 <-
    ggplot2::ggplot() +
    # ggplot2::scale_x_log10() +
    ggplot2::theme_linedraw() +
    ggplot2::theme(axis.line = ggplot2::element_line()) +
    ggplot2::labs(x = "Days since fever onset", y = "Antibody concentration") +
    ggplot2::ggtitle("Antibody Response Curve") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 20, face = "bold")
    )

  if (log_y) {
    plot1 <-
      plot1 +
      ggplot2::scale_y_log10(labels = scales::label_comma(),
                             minor_breaks = NULL)
  }

  layer_function <- function(cur_iter) {
    cur_params <- object |> filter(.data$iter == cur_iter)
    ggplot2::geom_function(
      alpha = alpha,
      fun = ab0,
      args = list(curve_params = cur_params),
      n = n_points,
      ...
    )
  }

  layers <-
    lapply(X = iters_to_graph, FUN = layer_function)

  plot1 <- plot1 + layers

  if (log_x) {
    plot1 <- plot1 +
      ggplot2::scale_x_log10(limits = xlim, labels = scales::label_comma())
  } else {
    plot1 <- plot1 + ggplot2::xlim(xlim)
  }

  return(plot1)
}
