#' Fetch and aggregate CRAN download counts
#'
#' Uses [packageRank::cranDownloads()] to fetch daily CRAN
#' download data with cumulative counts, then aggregates by
#' `unit`.
#'
#' @param unit Character string passed to [cut.Date()] for
#'   time aggregation (e.g. `"month"`, `"week"`).
#'
#' @returns A tibble with columns `date`, `provider`, `new`,
#'   and `cumulative`.
#'
#' @noRd
.fetch_cran_downloads <- function(unit) {
  dl <- packageRank::cranDownloads(
    "serocalculator",
    from = "2022-03-30",
    to = Sys.Date()
  )

  dplyr::tibble(
    date = dl$cranlogs.data$date,
    provider = "CRAN",
    new = dl$cranlogs.data$count,
    cumulative = dl$cranlogs.data$cumulative
  ) |>
    .aggregate_by_unit(unit)
}
