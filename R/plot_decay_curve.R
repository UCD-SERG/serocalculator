#' Plot a decay function
#'
#' @param decay_function a function with first argument `t`
#' @param xmax upper limit of x axis
#' @param ymax upper limit of y axis
#' @param title plot title
#' @param ... parameters passed to `decay_function`
#'
#' @returns a [ggplot2::ggplot]
#' @export
#'
#' @examples
#' plot_decay_curve(antibody_decay_curve)
plot_decay_curve <- function(
  decay_function = antibody_decay_curve,
  ...,
  xmax = 100,
  ymax = NA,
  title = ""
) {

  plot1 <-
    ggplot2::ggplot() +
    ggplot2::geom_function(
      n = 1001,
      fun = function(x) decay_function(t = x, ...),
    ) +
    ggplot2::theme_bw() +
    ggplot2::xlim(0, 100) +
    ggplot2::ylab("concentration") +
    ggplot2::xlab("time since infection (days)") +
    ggplot2::expand_limits(y = 10^-4) +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma()
    ) +
    ggplot2::ggtitle(title)

  if (!is.na(ymax)) {
    plot1 <- plot1 +
      ggplot2::expand_limits(y = ymax)
  }

  return(plot1)


}
