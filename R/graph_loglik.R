#' Graph log-likelihood of data
#'
#' @param highlight_points a possible highlighted value
#' @param x sequence of lambda values to graph
#' @param highlight_point_names labels for highlighted points
#' @param log_x should the x-axis be on a logarithmic scale (`TRUE`) or linear scale (`FALSE`, default)?
#' @inheritParams llik
#' @inheritDotParams llik -lambda
#' @return a [ggplot2::ggplot()]
#' @export
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
    ...)
{

  if(!is.list(curve_params) &&
     !is.element("d", names(curve_params)))
  {
    curve_params =
      curve_params |>
      dplyr::mutate(
        alpha = .data$alpha * 365.25,
        d = .data$r - 1)
  }

  plot1 = tibble(
    x = x |> sort(),
    y = llik(
      pop_data = pop_data,
      curve_params = curve_params,
      noise_params = noise_params,
      antigen_isos = antigen_isos,
      lambda = x,
      ...)
  ) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$y)) +
    # ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::xlab("incidence rate (events per person:year)") +
    ggplot2::ylab("log(likelihood)") +
    ggplot2::theme_bw() +
    ggplot2::labs(col = "") +
    ggplot2::theme(legend.position="bottom")

  if(!is.null(highlight_points))
  {

    plot1 = plot1 +
      ggplot2::geom_point(
        data = tibble(
          x = highlight_points,
          y = llik(
            pop_data = pop_data,
            curve_params = curve_params,
            noise_params = noise_params,
            antigen_isos = antigen_isos,
            lambda = highlight_points,
            ...)),
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          col = highlight_point_names)
      )
  }

  if(log_x)
  {
    plot1 = plot1 +
      ggplot2::scale_x_continuous(
        trans = "log10",
        labels = scales::label_comma())
  }
  return(plot1)
}
