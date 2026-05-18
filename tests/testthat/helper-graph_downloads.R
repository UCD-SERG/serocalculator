cran_downloads_fixture <- function() {
  "fixtures" |>
    testthat::test_path("cran_downloads.rds") |>
    readRDS()
}

mock_github_data <- function() {
  # Real GitHub data has zero downloads (no release assets),
  # so we use synthetic data for meaningful plot tests.
  new_downloads <- c(30L, 0L, 20L, 0L, 10L, 0L)

  dplyr::tibble(
    date = c(
      "2025-01-01", "2025-02-01", "2025-03-01",
      "2025-04-01", "2025-05-01", "2025-06-01"
    ) |>
      as.Date(),
    provider = "GitHub",
    new = new_downloads,
    cumulative = new_downloads |> cumsum()
  )
}
