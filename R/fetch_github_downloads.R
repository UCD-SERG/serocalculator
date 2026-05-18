#' Fetch and aggregate GitHub release download counts
#'
#' Queries the GitHub API for all releases of the
#' `serocalculator` package, computes daily new and cumulative
#' downloads, then aggregates by `unit`.
#'
#' @param unit Character string passed to [cut.Date()] for
#'   time aggregation (e.g. `"month"`, `"week"`).
#'
#' @returns A tibble with columns `date`, `provider`, `new`,
#'   and `cumulative`.
#'
#' @keywords internal
.fetch_github_downloads <- function(unit) {
  if (!requireNamespace("gh", quietly = TRUE)) {
    msg <- paste(
      "Package {.pkg gh} is required.",
      "Install with",
      "{.code install.packages('gh')}."
    )
    cli::cli_abort(msg)
  }

  releases <- gh::gh(
    "/repos/{owner}/{repo}/releases",
    owner = "UCD-SERG",
    repo = "serocalculator",
    .limit = Inf
  )

  github_releases <- purrr::map(
    releases,
    function(release) {
      tibble::tibble(
        date = release$published_at |> as.Date(),
        downloads = release$assets |>
          purrr::map_int("download_count") |>
          sum()
      )
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$date)

  today <- Sys.Date()
  start_date <- github_releases$date |> min()
  all_dates <- seq(start_date, today, by = "day")

  github_releases |>
    dplyr::mutate(
      new = .data$downloads,
      cumulative = cumsum(.data$downloads)
    ) |>
    dplyr::select("date", "new", "cumulative") |>
    tidyr::complete(date = all_dates) |>
    tidyr::fill("cumulative", .direction = "down") |>
    dplyr::mutate(
      cumulative = .data$cumulative |>
        tidyr::replace_na(0L),
      new = .data$new |>
        tidyr::replace_na(0L),
      provider = "GitHub"
    ) |>
    .aggregate_by_unit(unit)
}
