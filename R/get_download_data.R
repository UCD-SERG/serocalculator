#' Fetch and prepare download data for plotting
#'
#' Fetches CRAN (and optionally GitHub) download data,
#' then combines, filters, and pivots into long format.
#'
#' @param github Logical; include GitHub release downloads?
#'   Defaults to `FALSE`.
#' @param new Logical; include new (daily) downloads?
#'   Defaults to `TRUE`.
#' @param cumulative Logical; include cumulative downloads?
#'   Defaults to `TRUE`.
#' @param start Start date for the plot (a [Date] or string
#'   coercible to one). Defaults to `NULL` (all available
#'   data).
#' @param unit Character string specifying the time unit to
#'   aggregate by. One of `"day"`, `"week"`, `"month"`,
#'   `"quarter"`, or `"year"`. Defaults to `"month"`.
#' @param title Character string for the plot title. Defaults
#'   to a description including the time unit. Set to `NULL`
#'   to omit.
#' @inheritDotParams packageRank::cranDownloads
#'
#' @returns A long-format tibble with columns `date`,
#'   `provider`, `metric`, and `downloads`, plus attributes
#'   `default_title`, `github`, and `multi_metric`.
#'
#' @keywords internal
.get_download_data <- function(
  github = FALSE,
  new = TRUE,
  cumulative = TRUE,
  start = NULL,
  unit = c("month", "day", "week", "quarter", "year"),
  title,
  ...
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
  if (!missing(title)) {
    default_title <- title
  }
  attr(result, "default_title") <- default_title
  attr(result, "github") <- github
  attr(result, "multi_metric") <- new && cumulative

  result
}
