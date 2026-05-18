.aggregate_by_unit <- function(data, unit) {
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
