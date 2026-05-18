.fetch_cran_downloads <- function(unit) {
  cran_raw <- cranlogs::cran_downloads(
    packages = "serocalculator",
    from = "2022-03-30",
    to = Sys.Date()
  )

  dplyr::tibble(
    date = cran_raw$date,
    provider = "CRAN",
    new = cran_raw$count,
    cumulative = cumsum(cran_raw$count)
  ) |>
    .aggregate_by_unit(unit)
}
