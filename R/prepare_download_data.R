.prepare_download_data <- function(
  cran_data, github_data, start, metrics
) {
  metric_labels <- c(
    new = "New downloads",
    cumulative = "Cumulative downloads"
  )

  dplyr::bind_rows(cran_data, github_data) |>
    dplyr::filter(
      is.null(start) | .data$date >= as.Date(start)
    ) |>
    dplyr::select(
      "date", "provider", dplyr::all_of(metrics)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(metrics),
      names_to = "metric",
      values_to = "downloads"
    ) |>
    dplyr::mutate(
      metric = factor(
        .data$metric,
        levels = metrics,
        labels = metric_labels[metrics]
      )
    )
}
