#' Combine, filter, and pivot download data for plotting
#'
#' @param cran_data Tibble of CRAN download data from
#'   [.fetch_cran_downloads()].
#' @param github_data Tibble of GitHub download data from
#'   [.fetch_github_downloads()], or `NULL`.
#' @param start Start date for filtering, or `NULL` for no
#'   filtering.
#' @param metrics Character vector of metric columns to
#'   include (subset of `c("new", "cumulative")`).
#'
#' @returns A long-format tibble with columns `date`,
#'   `provider`, `metric`, and `downloads`.
#'
#' @keywords internal
.prepare_download_data <- function(
  cran_data, github_data, start, metrics
) {
  metric_labels <- c(
    new = "New downloads",
    cumulative = "Cumulative downloads"
  )

  combined <- dplyr::bind_rows(cran_data, github_data)

  if (!is.null(start)) {
    start_date <- start |> as.Date()
    combined <- combined |>
      dplyr::filter(.data$date >= start_date)
  }

  metric_cols <- dplyr::all_of(metrics)
  labels <- metric_labels[metrics]

  combined |>
    dplyr::select("date", "provider", metric_cols) |>
    tidyr::pivot_longer(
      cols = metric_cols,
      names_to = "metric",
      values_to = "downloads"
    ) |>
    dplyr::mutate(
      metric = .data$metric |>
        factor(levels = metrics, labels = labels)
    )
}
