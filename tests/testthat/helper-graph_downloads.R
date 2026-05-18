cran_downloads_fixture <- function() {
  readRDS(testthat::test_path("fixtures", "cran_downloads.rds"))
}

mock_github_data <- function() {
  # Real GitHub data has zero downloads (no release assets),

  # so we use synthetic data for meaningful plot tests.
  dplyr::tibble(
    date = as.Date(c(
      "2025-01-01", "2025-02-01", "2025-03-01",
      "2025-04-01", "2025-05-01", "2025-06-01"
    )),
    provider = "GitHub",
    new = c(30L, 0L, 20L, 0L, 10L, 0L),
    cumulative = cumsum(
      c(30L, 0L, 20L, 0L, 10L, 0L)
    )
  )
}
