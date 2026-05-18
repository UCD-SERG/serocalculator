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

    daily |>
      serocalculator:::.aggregate_by_unit("month") |>
      expect_snapshot_value(style = "json2")
  }
)

test_that(
  desc = ".prepare_download_data() pivots correctly",
  code = {
    cran <- cran_downloads_fixture()
    cran |>
      serocalculator:::.prepare_download_data(
        NULL, NULL, c("new", "cumulative")
      ) |>
      expect_snapshot_value(style = "json2")
  }
)

test_that(
  desc = ".prepare_download_data() filters by start date",
  code = {
    cran <- cran_downloads_fixture()
    cran |>
      serocalculator:::.prepare_download_data(
        NULL, "2026-01-01", c("new")
      ) |>
      expect_snapshot_value(style = "json2")
  }
)

test_that(
  desc = ".prepare_download_data() includes only requested metrics",
  code = {
    cran <- cran_downloads_fixture()
    cran |>
      serocalculator:::.prepare_download_data(
        NULL, NULL, "new"
      ) |>
      expect_snapshot_value(style = "json2")
  }
)

test_that(
  desc = "autoplot.download_data() returns ggplot for CRAN only",
  code = {
    cran <- cran_downloads_fixture()
    download_data <- cran |>
      serocalculator:::.prepare_download_data(
        NULL, NULL, c("new", "cumulative")
      )
    attr(download_data, "github") <- FALSE
    attr(download_data, "multi_metric") <- TRUE
    attr(download_data, "title") <- "Test"
    class(download_data) <- c("download_data", class(download_data))

    download_data |>
      ggplot2::autoplot() |>
      vdiffr::expect_doppelganger(
        title = "downloads-cran-only"
      )
  }
)

test_that(
  desc = "autoplot.download_data() returns ggplot for CRAN + GitHub",
  code = {
    cran <- cran_downloads_fixture()
    gh <- mock_github_data()
    download_data <- cran |>
      serocalculator:::.prepare_download_data(
        gh, NULL, c("new", "cumulative")
      )
    attr(download_data, "github") <- TRUE
    attr(download_data, "multi_metric") <- TRUE
    attr(download_data, "title") <- "Test"
    class(download_data) <- c("download_data", class(download_data))

    download_data |>
      ggplot2::autoplot() |>
      vdiffr::expect_doppelganger(
        title = "downloads-cran-github"
      )
  }
)

test_that(
  desc = "autoplot.download_data() handles single metric",
  code = {
    cran <- cran_downloads_fixture()
    download_data <- cran |>
      serocalculator:::.prepare_download_data(
        NULL, NULL, "cumulative"
      )
    attr(download_data, "github") <- FALSE
    attr(download_data, "multi_metric") <- FALSE
    attr(download_data, "title") <- NULL
    class(download_data) <- c("download_data", class(download_data))

    download_data |>
      ggplot2::autoplot() |>
      vdiffr::expect_doppelganger(
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
