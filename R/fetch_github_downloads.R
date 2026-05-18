.fetch_github_downloads <- function(unit) {
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
        date = as.Date(release$published_at),
        downloads = sum(
          purrr::map_int(
            release$assets, "download_count"
          )
        )
      )
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$date)

  github_releases |>
    dplyr::mutate(
      new = .data$downloads,
      cumulative = cumsum(.data$downloads)
    ) |>
    dplyr::select("date", "new", "cumulative") |>
    tidyr::complete(
      date = seq(min(.data$date), Sys.Date(), by = "day")
    ) |>
    tidyr::fill("cumulative", .direction = "down") |>
    dplyr::mutate(
      cumulative = tidyr::replace_na(
        .data$cumulative, 0L
      ),
      new = tidyr::replace_na(.data$new, 0L),
      provider = "GitHub"
    ) |>
    .aggregate_by_unit(unit)
}
