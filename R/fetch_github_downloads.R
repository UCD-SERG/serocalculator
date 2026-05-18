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
        date = release$published_at |> substr(1, 10) |> as.Date(),
        downloads = release$assets |>
          purrr::map_int("download_count") |>
          sum()
      )
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$date)

  if (nrow(github_releases) == 0L) {
    return(tibble::tibble(
      date = as.Date(character()),
      provider = character(),
      new = integer(),
      cumulative = integer()
    ))
  }

  today <- Sys.Date()
  start_date <- github_releases$date |> min()
  all_dates <- seq(start_date, today, by = "day")

  github_releases |>
    dplyr::mutate(cumulative = cumsum(.data$downloads)) |>
    dplyr::rename(new = "downloads") |>
    tidyr::complete(date = all_dates) |>
    tidyr::fill("cumulative", .direction = "down") |>
    tidyr::replace_na(list(cumulative = 0L, new = 0L)) |>
    dplyr::mutate(provider = "GitHub") |>
    .aggregate_by_unit(unit)
}
