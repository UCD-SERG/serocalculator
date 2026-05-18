#' Build a ggplot of download data with adaptive faceting
#'
#' @param download_data Long-format tibble from
#'   [.get_download_data()], with attributes `github`,
#'   `multi_metric`, and `default_title`.
#' @param title Plot title. If missing, uses the
#'   `default_title` attribute from `download_data`.
#'   Set to `NULL` to omit.
#'
#' @returns A [ggplot2::ggplot()] object.
#'
#' @noRd
.plot_downloads <- function(download_data, title) {
  if (missing(title)) {
    title <- attr(download_data, "default_title")
  }
  github <- attr(download_data, "github")
  multi_metric <- attr(download_data, "multi_metric")
  mapping <- ggplot2::aes(
    x = .data$date, y = .data$downloads
  )
  p <- ggplot2::ggplot(download_data, mapping) +
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
