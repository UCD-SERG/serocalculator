#' Plot package download counts over time by source
#'
#' Plots new and cumulative download counts for this package.
#' Delegates to [gdl::graph_downloads()].
#'
#' @param package Character string; the CRAN package name. Defaults to
#'   `"serocalculator"`.
#' @param github_repo Character string; the GitHub repository in
#'   `"owner/repo"` format, used when `github = TRUE`. Defaults to
#'   `"UCD-SERG/serocalculator"`.
#' @param new Logical; include new (period) downloads? Defaults to `TRUE`.
#' @param cumulative Logical; include cumulative downloads? Defaults to `TRUE`.
#' @param unit Character string specifying the time unit to aggregate by.
#'   One of `"day"`, `"week"`, `"month"`, `"quarter"`, or `"year"`.
#'   Defaults to `"month"`.
#' @param start Start date for the plot (a [Date] or string coercible to
#'   one). Defaults to `NULL` (all available data).
#' @param title Character string for the plot title. Defaults to a
#'   description including the package name and time unit. Set to `NULL`
#'   to omit.
#' @param github Logical; include GitHub release downloads? Defaults to
#'   `FALSE`.
#'
#' @return A [ggplot2::ggplot()] object with faceted panels.
#'
#' @export
graph_downloads <- function(
  package = "serocalculator",
  github_repo = "UCD-SERG/serocalculator",
  new = TRUE,
  cumulative = TRUE,
  unit = c("month", "day", "week", "quarter", "year"),
  start = NULL,
  title,
  github = FALSE
) {
  unit <- match.arg(unit)
  args <- list(
    package = package,
    github_repo = if (github) github_repo else NULL,
    new = new,
    cumulative = cumulative,
    unit = unit,
    start = start
  )
  if (!missing(title)) {
    args$title <- title
  }
  do.call(gdl::graph_downloads, args)
}
