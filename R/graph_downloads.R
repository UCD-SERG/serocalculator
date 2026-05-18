#' Plot package download counts over time by source
#'
#' Fetches download data from CRAN (via `cranlogs`) and optionally GitHub
#' Releases (via `gh`), then plots new and cumulative downloads.
#'
#' @param github Logical; include GitHub release downloads? Defaults to
#'   `FALSE`.
#' @param new Logical; include new (daily) downloads? Defaults to `TRUE`.
#' @param cumulative Logical; include cumulative downloads? Defaults to `TRUE`.
#' @param start Start date for the plot (a [Date] or string coercible to one).
#'   Defaults to `NULL` (all available data).
#' @param unit Character string specifying the time unit to aggregate by.
#'   One of `"day"`, `"week"`, `"month"`, `"quarter"`, or `"year"`.
#'   Defaults to `"month"`.
#' @param title Character string for the plot title. Defaults to
#'   a description including the time unit. Set to `NULL` to omit.
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
graph_downloads <- function(
  github = FALSE,
  new = TRUE,
  cumulative = TRUE,
  start = NULL,
  unit = c("month", "day", "week", "quarter", "year"),
  title
) {
  unit <- match.arg(unit)
  if (missing(title)) {
    title <- paste0("Downloads of serocalculator package from CRAN, by ", unit)
  }
  if (!new && !cumulative) {
    cli::cli_abort(
      "At least one of {.arg new} or {.arg cumulative} must be {.val TRUE}."
    )
  }
  if (!requireNamespace("cranlogs", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg cranlogs} is required. Install with {.code install.packages('cranlogs')}."
    )
  }
  if (github && !requireNamespace("gh", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg gh} is required. Install with {.code install.packages('gh')}."
    )
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
  ) |>
    dplyr::mutate(period = as.Date(cut(.data$date, breaks = unit))) |>
    dplyr::summarise(
      new = sum(.data$new),
      cumulative = dplyr::last(.data$cumulative),
      .by = c("period", "source")
    ) |>
    dplyr::rename(date = "period")

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
      dplyr::arrange(.data$date)

    github_data <- github_releases |>
      dplyr::mutate(
        new = .data$downloads,
        cumulative = cumsum(.data$downloads)
      ) |>
      dplyr::select("date", "new", "cumulative") |>
      tidyr::complete(date = seq(min(.data$date), Sys.Date(), by = "day")) |>
      tidyr::fill("cumulative", .direction = "down") |>
      dplyr::mutate(
        cumulative = tidyr::replace_na(.data$cumulative, 0L),
        new = tidyr::replace_na(.data$new, 0L),
        source = "GitHub"
      ) |>
      dplyr::mutate(period = as.Date(cut(.data$date, breaks = unit))) |>
      dplyr::summarise(
        new = sum(.data$new),
        cumulative = dplyr::last(.data$cumulative),
        .by = c("period", "source")
      ) |>
      dplyr::rename(date = "period")
  }

  # Combine and pivot to long format for faceting
  metrics <- c(if (new) "new", if (cumulative) "cumulative")
  metric_labels <- c(new = "New downloads", cumulative = "Cumulative downloads")

  download_data <- dplyr::bind_rows(cran_data, if (github) github_data) |>
    dplyr::filter(is.null(start) | .data$date >= as.Date(start)) |>
    dplyr::select("date", "source", dplyr::all_of(metrics)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(metrics),
      names_to = "metric",
      values_to = "downloads"
    ) |>
    dplyr::mutate(
      metric = factor(.data$metric, levels = metrics, labels = metric_labels[metrics])
    )

  p <- ggplot2::ggplot(
    download_data,
    ggplot2::aes(x = .data$date, y = .data$downloads)
  ) +
    ggplot2::geom_line(linewidth = 0.4)

  multi_metric <- new && cumulative
  if (github && multi_metric) {
    p <- p +
      ggplot2::facet_grid(
        metric ~ source,
        scales = "free_y",
        switch = "y"
      )
  } else if (github) {
    p <- p +
      ggplot2::facet_wrap(
        ~source,
        ncol = 2,
        scales = "free_y",
        strip.position = "left"
      )
  } else if (multi_metric) {
    p <- p +
      ggplot2::facet_wrap(
        ~metric,
        ncol = 1,
        scales = "free_y",
        strip.position = "left"
      )
  }

  p +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.placement = "outside")
}
