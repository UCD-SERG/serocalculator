test_that(
  desc = ".aggregate_by_unit() aggregates correctly",
  code = {
    dates <- c(
      "2024-01-01", "2024-01-02", "2024-01-03",
      "2024-02-01", "2024-02-02"
    ) |>
      as.Date()

    daily <- dplyr::tibble(
      date = dates,
      provider = "CRAN",
      new = c(10L, 20L, 30L, 5L, 15L),
      cumulative = c(10L, 30L, 60L, 65L, 80L)
    )

    result <- daily |>
      serocalculator:::.aggregate_by_unit("month")

    result |>
      nrow() |>
      expect_equal(2)
    result$new |>
      expect_equal(c(60L, 20L))
    result$cumulative |>
      expect_equal(c(60L, 80L))
    expected_dates <- c("2024-01-01", "2024-02-01") |> as.Date()
    result$date |>
      expect_equal(expected_dates)
  }
)

test_that(
  desc = ".prepare_download_data() pivots correctly",
  code = {
    cran <- cran_downloads_fixture()
    result <- cran |>
      serocalculator:::.prepare_download_data(
        NULL, NULL, c("new", "cumulative")
      )

    col_names <- result |> names()
    "metric" %in% col_names |>
      expect_true()
    "downloads" %in% col_names |>
      expect_true()
    result$metric |>
      levels() |>
      expect_equal(c("New downloads", "Cumulative downloads"))
  }
)

test_that(
  desc = ".prepare_download_data() filters by start date",
  code = {
    cran <- cran_downloads_fixture()
    result <- cran |>
      serocalculator:::.prepare_download_data(
        NULL, "2026-01-01", c("new")
      )

    cutoff <- "2026-01-01" |> as.Date()
    result$date |>
      min() |>
      expect_gte(cutoff)
  }
)

test_that(
  desc = ".prepare_download_data() includes only requested metrics",
  code = {
    cran <- cran_downloads_fixture()
    result <- cran |>
      serocalculator:::.prepare_download_data(
        NULL, NULL, "new"
      )

    result$metric |>
      levels() |>
      expect_equal("New downloads")
  }
)

test_that(
  desc = ".plot_downloads() returns ggplot for CRAN only",
  code = {
    cran <- cran_downloads_fixture()
    download_data <- cran |>
      serocalculator:::.prepare_download_data(
        NULL, NULL, c("new", "cumulative")
      )

    p <- download_data |>
      serocalculator:::.plot_downloads(
        github = FALSE,
        multi_metric = TRUE,
        title = "Test"
      )

    p |> expect_s3_class("ggplot")
    p |> vdiffr::expect_doppelganger(
      title = "downloads-cran-only"
    )
  }
)

test_that(
  desc = ".plot_downloads() returns ggplot for CRAN + GitHub",
  code = {
    cran <- cran_downloads_fixture()
    gh <- mock_github_data()
    download_data <- cran |>
      serocalculator:::.prepare_download_data(
        gh, NULL, c("new", "cumulative")
      )

    p <- download_data |>
      serocalculator:::.plot_downloads(
        github = TRUE,
        multi_metric = TRUE,
        title = "Test"
      )

    p |> expect_s3_class("ggplot")
    p |> vdiffr::expect_doppelganger(
      title = "downloads-cran-github"
    )
  }
)

test_that(
  desc = ".plot_downloads() handles single metric",
  code = {
    cran <- cran_downloads_fixture()
    download_data <- cran |>
      serocalculator:::.prepare_download_data(
        NULL, NULL, "cumulative"
      )

    p <- download_data |>
      serocalculator:::.plot_downloads(
        github = FALSE,
        multi_metric = FALSE,
        title = NULL
      )

    p |> expect_s3_class("ggplot")
    p |> vdiffr::expect_doppelganger(
      title = "downloads-cumulative-only"
    )
  }
)

test_that(
  desc = "graph_downloads() errors when both metrics are FALSE",
  code = {
    graph_downloads(new = FALSE, cumulative = FALSE) |>
      expect_error()
  }
)
