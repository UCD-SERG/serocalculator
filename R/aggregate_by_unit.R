#' Aggregate download data by a calendar unit
#'
#' Sums new downloads and takes the last cumulative value
#' within each time period.
#'
#' @param data A tibble with columns `date`, `provider`,
#'   `new`, and `cumulative`.
#' @param unit Character string passed to [cut.Date()] for
#'   time aggregation. One of `"month"`, `"day"`, `"week"`,
#'   `"quarter"`, or `"year"`.
#'
#' @returns A tibble with columns `date`, `provider`, `new`,
#'   and `cumulative`, aggregated by `unit`.
#'
#' @noRd
.aggregate_by_unit <- function(
  data,
  unit = c("month", "day", "week", "quarter", "year")
) {
  unit <- match.arg(unit)
  data |>
    dplyr::mutate(
      period = .data$date |>
        cut(breaks = unit) |>
        as.Date()
    ) |>
    dplyr::summarise(
      new = sum(.data$new),
      cumulative = dplyr::last(.data$cumulative),
      .by = c("period", "provider")
    ) |>
    dplyr::rename(date = "period")
}
