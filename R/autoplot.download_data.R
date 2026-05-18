#' Plot download data with adaptive faceting
#'
#' @param object A `download_data` object from
#'   [.get_download_data()].
#' @param ... Not currently used.
#'
#' @returns A [ggplot2::ggplot()] object.
#'
#' @export
#' @keywords internal
autoplot.download_data <- function(object, ...) {
  title <- attr(object, "title")
  github <- attr(object, "github")
  multi_metric <- attr(object, "multi_metric")
  mapping <- ggplot2::aes(
    x = .data$date, y = .data$downloads
  )
  p <- ggplot2::ggplot(object, mapping) +
    ggplot2::geom_line(linewidth = 0.4)

  if (github && multi_metric) {
    p <- p +
      ggplot2::facet_grid(
        metric ~ provider,
        scales = "free_y",
        switch = "y"
      )
  } else if (github) {
    p <- p +
      ggplot2::facet_wrap(
        ~provider,
        ncol = 2,
        scales = "free_y",
        strip.position = "left"
      )
  } else if (multi_metric) {
    p <- p +
      ggplot2::facet_wrap(
        ~metric,
        ncol = 1,
        scales = "free_y",
        strip.position = "left"
      )
  }

  comma_labels <- scales::label_comma()

  p +
    ggplot2::scale_y_continuous(labels = comma_labels) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.placement = "outside")
}
