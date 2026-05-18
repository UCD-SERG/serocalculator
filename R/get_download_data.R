#' Fetch and prepare download data for plotting
#'
#' Fetches CRAN (and optionally GitHub) download data,
#' then combines, filters, and pivots into long format.
#'
#' @inheritParams graph_downloads
#' @inheritDotParams .fetch_cran_downloads
#'
#' @returns A long-format tibble with columns `date`,
#'   `provider`, `metric`, and `downloads`.
#'
#' @noRd
.get_download_data <- function(
  github, new, cumulative, start, unit, ...
) {
  if (!new && !cumulative) {
    msg <- paste(
      "At least one of {.arg new} or",
      "{.arg cumulative} must be {.val TRUE}."
    )
    cli::cli_abort(msg)
  }

  cran_data <- .fetch_cran_downloads(unit, ...)
  github_data <- if (github) .fetch_github_downloads(unit)

  metrics <- c(
    if (new) "new",
    if (cumulative) "cumulative"
  )

  result <- .prepare_download_data(
    cran_data, github_data, start, metrics
  )

  resolved_unit <- unit |> match.arg(
    choices = c("month", "day", "week", "quarter", "year")
  )
  default_title <- paste0(
    "Downloads of serocalculator package from CRAN, by ",
    resolved_unit
  )
  attr(result, "default_title") <- default_title
  attr(result, "github") <- github
  attr(result, "multi_metric") <- new && cumulative

  result
}
