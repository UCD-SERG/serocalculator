.aggregate_by_unit <- function(data, unit) {
  data |>
    dplyr::mutate(
      period = as.Date(cut(.data$date, breaks = unit))
    ) |>
    dplyr::summarise(
      new = sum(.data$new),
      cumulative = dplyr::last(.data$cumulative),
      .by = c("period", "provider")
    ) |>
    dplyr::rename(date = "period")
}
