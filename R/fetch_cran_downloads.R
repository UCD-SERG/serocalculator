#' Fetch and aggregate CRAN download counts
#'
#' Uses [packageRank::cranDownloads()] to fetch daily CRAN
#' download data with cumulative counts, then aggregates by
#' `unit`.
#'
#' @param unit Character string passed to [cut.Date()] for
#'   time aggregation (e.g. `"month"`, `"week"`).
#' @param ... Additional arguments passed to
#'   [packageRank::cranDownloads()].
#'
#' @returns A tibble with columns `date`, `provider`, `new`,
#'   and `cumulative`.
#'
#' @noRd
.fetch_cran_downloads <- function(unit, ...) {
  if (!requireNamespace("packageRank", quietly = TRUE)) {
    msg <- paste(
      "Package {.pkg packageRank} is required.",
      "Install with",
      "{.code install.packages('packageRank')}."
    )
    cli::cli_abort(msg)
  }

  today <- Sys.Date()
  dl <- packageRank::cranDownloads(
    "serocalculator",
    from = "2022-03-30",
    to = today,
    ...
  )

  dl$cranlogs.data |>
    dplyr::rename(new = "count") |>
    dplyr::mutate(provider = "CRAN") |>
    dplyr::select("date", "provider", "new", "cumulative") |>
    .aggregate_by_unit(unit)
}
