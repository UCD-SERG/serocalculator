mock_cran_data <- function(unit = "month") {
  dates <- seq(
    as.Date("2024-01-01"),
    as.Date("2024-06-30"),
    by = "day"
  )
  daily <- dplyr::tibble(
    date = dates,
    provider = "CRAN",
    new = rep(c(5L, 10L, 3L, 8L, 2L, 0L, 1L), length.out = length(dates)),
    cumulative = cumsum(
      rep(c(5L, 10L, 3L, 8L, 2L, 0L, 1L), length.out = length(dates))
    )
  )
  serocalculator:::.aggregate_by_unit(daily, unit)
}

mock_github_data <- function(unit = "month") {
  dates <- seq(
    as.Date("2024-01-01"),
    as.Date("2024-06-30"),
    by = "day"
  )
  daily <- dplyr::tibble(
    date = dates,
    provider = "GitHub",
    new = rep(0L, length(dates)),
    cumulative = rep(50L, length(dates))
  )
  # simulate two releases
  daily$new[1] <- 30L
  daily$new[91] <- 20L
  daily$cumulative <- cumsum(daily$new)

  serocalculator:::.aggregate_by_unit(daily, unit)
}
