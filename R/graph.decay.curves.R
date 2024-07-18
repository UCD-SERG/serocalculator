#' @title Graph an antibody decay curve model
#'
#' @param object a [data.frame()] of curve parameters (one or more MCMC samples)
#' @param verbose verbose output
#' @param xlim range of x values to graph
#' @param n_curves how many curves to plot (see details).
#' @param n_points Number of points to interpolate along the x axis (passed to [ggplot2::geom_function()])
#' @param rows_to_graph which rows of `curve_params` to plot (overrides `n_curves`).
#' @param alpha (passed to [ggplot2::geom_function()]) how transparent the curves should be:
#' * 0 = fully transparent (invisible)
#' * 1 = fully opaque
#' @param log_x should the x-axis be on a logarithmic scale (`TRUE`) or linear scale (`FALSE`, default)?
#' @param log_y should the y-axis be on a logarithmic scale (default, `TRUE`) or linear scale (`FALSE`)?
#' @inheritParams ggplot2::geom_function
#' @inheritDotParams ggplot2::geom_function
#' @returns a [ggplot2::ggplot()] object
#' @details
#' ## `n_curves` and `rows_to_graph`
#' In most cases, `curve_params` will contain too many rows of MCMC samples for all of these samples to be plotted at once.
#' * Setting the  `n_curves` argument to a value smaller than the number of rows in `curve_params` will cause this function to select the first `n_curves` rows to graph.
#' * Setting `n_curves` larger than the number of rows in ` will result all curves being plotted.
#' * If the user directly specifies the `rows_to_graph` argument, then `n_curves` has no effect.
#' @examples
#' library(dplyr) # loads the `%>%` operator and `dplyr::filter()`
#'
#' load_curve_params("https://osf.io/download/rtw5k/") %>%
#'   dplyr::filter(antigen_iso == "HlyE_IgG") %>%
#'   serocalculator:::plot_curve_params_one_ab()
#'
plot_curve_params_one_ab = function(
    object,
    verbose = FALSE,
    alpha = .4,
    n_curves = 100,
    n_points = 1000,
    log_x = FALSE,
    log_y = TRUE,
    rows_to_graph = 1:min(n_curves, nrow(object)),
    xlim = c(10^-1, 10^3.1),
    ...) {
  plot1 <-
    ggplot2::ggplot() +
    # ggplot2::scale_x_log10() +
    ggplot2::theme_linedraw() +
    ggplot2::theme(
      axis.line = ggplot2::element_line()
    ) +
    ggplot2::labs(
      x = "Days since fever onset",
      y = "Antibody Concentration"
    ) +
    ggplot2::ggtitle("Decay Curve") +
    ggplot2::theme(
      plot.title =
        ggplot2::element_text(
          size = 20,
          face = "bold"
        )
    )

  if (log_y) {
    plot1 <- plot1 +
      ggplot2::scale_y_log10(
        # limits = c(0.9, 2000),
        labels = scales::label_comma(),
        # breaks = c(0.1, 1, 10, 100, 1000),
        minor_breaks = NULL
      )
  }

  layer_function <- function(cur_row) {
    cur_params <- object[cur_row, ]
    ggplot2::geom_function(
      alpha = alpha,
      # aes(color = cur_row),
      fun = ab0,
      n = n_points,
      args = list(curve_params = cur_params),
      ...)
  }

  layers <-
    lapply(
      X = rows_to_graph,
      FUN = layer_function
    )

  plot1 <- plot1 + layers

  if (log_x) {
    plot1 <- plot1 +
      ggplot2::scale_x_log10(
        limits = xlim,
        labels = scales::label_comma()
      )
  } else {
    plot1 <- plot1 + ggplot2::xlim(xlim)
  }

  return(plot1)
}


# ggplot() +
#   geom_line(data = serocourse.all, aes(x= t, y = res, group = iter)) +
#   facet_wrap(~antigen_iso, ncol=2) +
#   scale_y_log10(limits = c(0.9, 2000), breaks = c(1, 10, 100, 1000), minor_breaks = NULL) +
#   theme_minimal()  +
#   theme(axis.line=element_line()) +
#   labs(x="Days since fever onset", y="ELISA units")

# mcmc %>% ungroup() %>% slice_head(by = antigen_iso, n = 10) %>% droplevels() %>% plot_curve_params_one_ab(alpha  = .4) %>% print()
