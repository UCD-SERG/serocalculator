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
#' @returns A `download_data` tibble (subclass of
#'   `tbl_df`) with columns `date`, `provider`, `metric`,
#'   and `downloads`, plus attributes `title`,
#'   `github`, and `multi_metric`.
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

  auto_title <- paste0(
    "Downloads of serocalculator package from CRAN, by ",
    unit[1]
  )
  if (!missing(title)) {
    auto_title <- title
  }

  result |>
    structure(
      title = auto_title,
      github = github,
      multi_metric = new && cumulative
    ) |>
    .subclass("download_data")
}
