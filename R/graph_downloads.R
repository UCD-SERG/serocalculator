#' Plot package download counts over time by source
#'
#' Fetches download data from CRAN (via `cranlogs`) and optionally GitHub
#' Releases (via `gh`), then plots new and cumulative downloads.
#'
#' @param github Logical; include GitHub release downloads? Defaults to
#'   `FALSE`.
#'
#' @return A [ggplot2::ggplot()] object with faceted panels.
#'
#' @details
#' GitHub release downloads are cumulative counts per release asset from the
#' GitHub API. New GitHub downloads are derived as the contribution of each
#' release. CRAN downloads reflect daily counts from the `cranlogs` API.
#'
#' Requires the `cranlogs` package (and `gh` if `github = TRUE`), listed
#' under `Suggests`.
#'
#' @export
#' @keywords internal
graph_downloads <- function(github = FALSE) {
  if (!requireNamespace("cranlogs", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg cranlogs} is required. Install with {.code install.packages('cranlogs')}.")
  }
  if (github && !requireNamespace("gh", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg gh} is required. Install with {.code install.packages('gh')}.")
  }

  pkg <- "serocalculator"

  # CRAN data: daily counts + cumulative
  cran_raw <- cranlogs::cran_downloads(
    packages = pkg,
    from = "2022-03-30",
    to = Sys.Date()
  )

  cran_data <- dplyr::tibble(
    date = cran_raw$date,
    source = "CRAN",
    new = cran_raw$count,
    cumulative = cumsum(cran_raw$count)
  )

  if (github) {
    # GitHub data: per-release download totals -> cumulative + new
    releases <- gh::gh(
      "/repos/{owner}/{repo}/releases",
      owner = "UCD-SERG",
      repo = pkg,
      .limit = Inf
    )

    github_releases <- purrr::map(releases, function(release) {
      tibble::tibble(
        date = as.Date(release$published_at),
        downloads = purrr::map_int(release$assets, "download_count") |> sum()
      )
    }) |>
      purrr::list_rbind() |>
      dplyr::arrange(date)

    github_data <- github_releases |>
      dplyr::mutate(cumulative = cumsum(downloads)) |>
      tidyr::complete(date = seq(min(date), Sys.Date(), by = "day")) |>
      tidyr::fill(cumulative, .direction = "down") |>
      dplyr::mutate(
        cumulative = tidyr::replace_na(cumulative, 0L),
        new = c(0, pmax(diff(cumulative), 0)),
        source = "GitHub"
      )
  }

  # Combine and pivot to long format for faceting
  download_data <- dplyr::bind_rows(cran_data, if (github) github_data) |>
    dplyr::select(date, source, new, cumulative) |>
    tidyr::pivot_longer(
      cols = c(new, cumulative),
      names_to = "metric",
      values_to = "downloads"
    ) |>
    dplyr::mutate(
      metric = factor(
        metric,
        levels = c("new", "cumulative"),
        labels = c("New downloads", "Cumulative downloads")
      )
    )

  p <- ggplot2::ggplot(download_data, ggplot2::aes(x = date, y = downloads)) +
    ggplot2::geom_line(linewidth = 0.4)

  if (github) {
    p <- p + ggplot2::facet_grid(
      metric ~ source,
      scales = "free_y",
      switch = "y"
    )
  } else {
    p <- p + ggplot2::facet_wrap(
      ~metric,
      ncol = 1,
      scales = "free_y",
      strip.position = "left"
    )
  }

  p +
    ggplot2::labs(
      title = "serocalculator downloads over time",
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.placement = "outside")
}
