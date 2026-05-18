.plot_downloads <- function(
  download_data, github, multi_metric, title
) {
  p <- ggplot2::ggplot(
    download_data,
    ggplot2::aes(x = .data$date, y = .data$downloads)
  ) +
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

  p +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma()
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.placement = "outside")
}
