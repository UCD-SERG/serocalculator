#' Graph log-likelihood of data
#'
#' @param highlight_points a possible highlighted value
#' @param x sequence of lambda values to graph
#' @param highlight_point_names labels for highlighted points
#' @param log_x should the x-axis be on a logarithmic scale (`TRUE`) or linear scale (`FALSE`, default)?
#' @inheritDotParams llik
#' @return a [ggplot2::ggplot()]
#' @export
#'
graph_loglik = function(
    ...,
    x = 10^seq(-3, 0, by = .1),
    highlight_points = NULL,
    highlight_point_names = "highlight_points",
    log_x = FALSE)
{

  plot1 = tibble(
    x = x |> sort(),
    y = llik(lambda = x, ...)
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
          y = llik(lambda = highlight_points, ...)),
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
