#' Graph log-likelihood of data
#'
#' @param highlight_points a possible highlighted value
#' @param x sequence of lambda values to graph
#' @param highlight_point_names labels for highlighted points
#' @param log_x should the x-axis be on a logarithmic scale (`TRUE`) or linear scale (`FALSE`, default)?
#' @param previous_plot if not NULL, the current data is added to the existing graph
#' @param curve_label if not NULL, add a label for the curve
#' @inheritParams log_likelihood
#' @inheritDotParams log_likelihood -lambda
#' @return a [ggplot2::ggplot()]
#' @export
#' @examples
#'
#' library(dplyr)
#' library(tibble)
#'
#' # Load cross-sectional data
#' xs_data <- load_pop_data("https://osf.io/download//n6cp3/")
#'
#'
#' # Load curve parameters and subset for the purposes of this example
#' dmcmc <- load_curve_params("https://osf.io/download/rtw5k/") %>%
#'   filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) %>%
#'   slice(1:100, .by = antigen_iso)
#'
#' # Load noise parameters
#' cond <- tibble(
#'   antigen_iso = c("HlyE_IgG", "HlyE_IgA"),
#'   nu = c(0.5, 0.5),                          # Biologic noise (nu)
#'   eps = c(0, 0),                             # M noise (eps)
#'   y.low = c(1, 1),                           # Low cutoff (llod)
#'   y.high = c(5e6, 5e6))                      # High cutoff (y.high)
#'
#' # Graph the log likelihood
#' lik_HlyE_IgA <- graph.loglik(
#'   pop_data = xs_data,
#'   curve_params = dmcmc,
#'   noise_params = cond,
#'   antigen_isos = "HlyE_IgA",
#'   log_x = TRUE
#' )
#'
#' lik_HlyE_IgA
#'
graph.loglik = function(
    pop_data,
    curve_params,
    noise_params,
    antigen_isos,
    x = 10^seq(-3, 0, by = .1),
    highlight_points = NULL,
    highlight_point_names = "highlight_points",
    log_x = FALSE,
    previous_plot = NULL,
    curve_label = paste(antigen_isos, collapse = " + "),
    ...) {
  if (!is.list(curve_params) &&
    !is.element("d", names(curve_params))) {
    curve_params <-
      curve_params %>%
      dplyr::mutate(
        alpha = .data$alpha * 365.25,
        d = .data$r - 1
      )
  }

  plot_data <-
    tibble(
      x = x %>% sort(),
      y = log_likelihood(
        pop_data = pop_data,
        curve_params = curve_params,
        noise_params = noise_params,
        antigen_isos = antigen_isos,
        lambda = x,
        ...
      )
    )

  if (is.null(previous_plot)) {
    plot1 <- plot_data %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$y)) +
      # ggplot2::geom_point() +
      ggplot2::geom_line(aes(color = curve_label)) +
      ggplot2::xlab("incidence rate (events per person:year)") +
      ggplot2::ylab("log(likelihood)") +
      ggplot2::theme_bw() +
      ggplot2::labs(col = "") +
      ggplot2::theme(legend.position = "bottom")

    if (!is.null(highlight_points)) {
      plot1 <- plot1 +
        ggplot2::geom_point(
          data = tibble(
            x = highlight_points,
            y = log_likelihood(
              pop_data = pop_data,
              curve_params = curve_params,
              noise_params = noise_params,
              antigen_isos = antigen_isos,
              lambda = highlight_points,
              ...
            )
          ),
          ggplot2::aes(
            x = .data$x,
            y = .data$y,
            col = highlight_point_names
          )
        )
    }

    if (log_x) {
      plot1 <- plot1 +
        ggplot2::scale_x_log10(
          labels = scales::label_comma()
        )
    }
  } else {
    plot1 <- previous_plot +
      ggplot2::geom_line(
        data = plot_data,
        aes(color = curve_label)
      )
  }
  return(plot1)
}
