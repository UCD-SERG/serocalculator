#' Graph an antibody decay curve model
#'
#' @param object a [data.frame()] of curve parameters (one or more MCMC samples)
#' @param verbose verbose output
#' @param xlim range of x values to graph
#' @param n_curves how many curves to plot (see details).
#' @param rows_to_graph which rows of `curve_params` to plot (overrides `n_curves`).
#' @param alpha (passed to [ggplot2::geom_function()]) how transparent the curves should be:
#' * 0 = fully transparent (invisible)
#' * 1 = fully opaque (no transparency)
#' @param log_x should the x-axis be on a logarithmic scale (`TRUE`) or linear scale (`FALSE`, default)?
#' @inheritParams ggplot2::geom_function
#' @returns a [ggplot2::ggplot()] object
#' @details
#' ## `n_curves` and `rows_to_graph`
#' In most cases, `curve_params` will contain too many rows of MCMC samples for all of these samples to be plotted at once.
#' * Setting the  `n_curves` argument to a value smaller than the number of rows in `curve_params` will cause this function to select the first `n_curves` rows to graph.
#' * Setting `n_curves` larger than the number of rows in ` will result all curves being plotted.
#' * If the user directly specifies the `rows_to_graph` argument, then `n_curves` has no effect.
#' @examples
#' \dontrun{
#' curve_params = readRDS(url("https://osf.io/download/rtw5k/"))
#' plot1 = graph.curve.params(curve_params)
#' print(plot1)
#' }
plot_curve_params_one_ab = function(
    object,
    verbose = FALSE,
    alpha = .4,
    n_curves = 100,
    log_x = FALSE,
    rows_to_graph = 1:min(n_curves, nrow(object)),
    xlim = c(10^-1, 10^3.1),
    ...)
{

  bt <- function(y0, y1, t1)
  {
    to_return = try(log(y1 / y0) / t1 )
    # if(inherits(to_return, "try-error")) browser()
    return(to_return)
  }

  # uses r > 1 scale for shape
  ab0 <- function(
    t,
    curve_params)

  {

    y0 = curve_params[["y0"]]
    y1 = curve_params[["y1"]]
    t1 = curve_params[["t1"]]
    alpha = curve_params[["alpha"]]
    shape = curve_params[["r"]]

    beta <- bt(y0, y1, t1)

    yt <- 0

    yt_phase_1 = y0 * exp(beta * t)
    yt_phase_2 = (y1 ^ (1 - shape) - (1 - shape) * alpha * (t - t1)) ^ (1 / (1 - shape))
    yt = dplyr::if_else(t <= t1, yt_phase_1, yt_phase_2)
    return(yt)

  }

  plot1 =
    ggplot2::ggplot() +
    ggplot2::scale_y_log10(
      # limits = c(0.9, 2000),
      labels = scales::label_comma(),
      # breaks = c(0.1, 1, 10, 100, 1000),
      minor_breaks = NULL
    ) +
    # ggplot2::scale_x_log10() +
    ggplot2::theme_minimal()  +
    ggplot2::theme(
      axis.line = ggplot2::element_line()) +
    ggplot2::labs(
      x = "Days since fever onset",
      y = "ELISA units")

  layer_function = function(cur_row)
  {
    cur_params = object[cur_row, ]
    ggplot2::geom_function(
        alpha = alpha,
        # aes(color = cur_row),
        fun = function(x) ab0(x, curve_params = cur_params))
  }



  layers =
    lapply(
      X = rows_to_graph,
      FUN = layer_function)

  plot1 = plot1 + layers

  if(log_x)
  {
    plot1 = plot1 +
      ggplot2::scale_x_log10(
      limits = xlim,
      labels = scales::label_comma())
  } else
  {
    plot1 = plot1 + ggplot2::xlim(xlim)

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
