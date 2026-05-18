#' Fetch and prepare download data for plotting
#'
#' Fetches CRAN (and optionally GitHub) download data,
#' then combines, filters, and pivots into long format.
#'
#' @param github Logical; whether to include GitHub data.
#' @param new Logical; whether to include new downloads.
#' @param cumulative Logical; whether to include cumulative
#'   downloads.
#' @param start Start date for filtering, or `NULL`.
#' @param unit Character string for time aggregation.
#' @param ... Additional arguments passed to
#'   [packageRank::cranDownloads()].
#'
#' @returns A long-format tibble with columns `date`,
#'   `provider`, `metric`, and `downloads`.
#'
#' @noRd
.get_download_data <- function(
  github, new, cumulative, start, unit, ...
) {
  cran_data <- .fetch_cran_downloads(unit, ...)
  github_data <- if (github) .fetch_github_downloads(unit)

  metrics <- c(
    if (new) "new",
    if (cumulative) "cumulative"
  )

  .prepare_download_data(
    cran_data, github_data, start, metrics
  )
}
