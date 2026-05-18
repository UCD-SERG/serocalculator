#' Plot package download counts over time by source
#'
#' Fetches download data from CRAN (via `cranlogs`) and optionally GitHub
#' Releases (via `gh`), then plots new and cumulative downloads.
#'
#' @inheritParams .get_download_data
#' @inheritDotParams .get_download_data
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
graph_downloads <- function(...) {
  .get_download_data(...) |> ggplot2::autoplot()
}
