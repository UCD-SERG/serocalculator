#' Plot package download counts over time by source
#'
#' Fetches download data from CRAN (via `cranlogs`) and optionally GitHub
#' Releases (via `gh`), then plots new and cumulative downloads.
#'
#' @param github Logical; include GitHub release downloads? Defaults to
#'   `FALSE`.
#' @param new Logical; include new (daily) downloads? Defaults to `TRUE`.
#' @param cumulative Logical; include cumulative downloads? Defaults to
#'   `TRUE`.
#' @param start Start date for the plot (a [Date] or string coercible to
#'   one). Defaults to `NULL` (all available data).
#' @param unit Character string specifying the time unit to aggregate by.
#'   One of `"day"`, `"week"`, `"month"`, `"quarter"`, or `"year"`.
#'   Defaults to `"month"`.
#' @param title Character string for the plot title. Defaults to
#'   a description including the time unit. Set to `NULL` to omit.
#' @inheritDotParams packageRank::cranDownloads
#'
#' @return A [ggplot2::ggplot()] object with faceted panels.
#'
#' @details
#' GitHub release downloads are cumulative counts per release asset
#' from the GitHub API. New GitHub downloads are derived as the
#' contribution of each release. CRAN downloads are fetched via
#' [packageRank::cranDownloads()].
#'
#' Requires the `packageRank` package (and `gh` if
#' `github = TRUE`), listed under `Suggests`.
#'
#' @export
#' @keywords internal
graph_downloads <- function(
  github = FALSE,
  new = TRUE,
  cumulative = TRUE,
  start = NULL,
  unit = c("month", "day", "week", "quarter", "year"),
  title,
  ...
) {
  download_data <- .get_download_data(
    github, new, cumulative, start, unit, ...
  )

  if (missing(title)) {
    .plot_downloads(
      download_data,
      github = github,
      multi_metric = new && cumulative
    )
  } else {
    .plot_downloads(
      download_data,
      github = github,
      multi_metric = new && cumulative,
      title = title
    )
  }
}
