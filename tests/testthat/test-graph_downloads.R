test_that(
  desc = ".aggregate_by_unit() aggregates correctly",
  code = {
    daily <- dplyr::tibble(
      date = as.Date(c(
        "2024-01-01", "2024-01-02", "2024-01-03",
        "2024-02-01", "2024-02-02"
      )),
      provider = "CRAN",
      new = c(10L, 20L, 30L, 5L, 15L),
      cumulative = c(10L, 30L, 60L, 65L, 80L)
    )

    result <- serocalculator:::.aggregate_by_unit(
      daily, "month"
    )

    expect_equal(nrow(result), 2)
    expect_equal(result$new, c(60L, 20L))
    expect_equal(result$cumulative, c(60L, 80L))
    expect_equal(
      result$date,
      as.Date(c("2024-01-01", "2024-02-01"))
    )
  }
)

test_that(
  desc = ".prepare_download_data() pivots correctly",
  code = {
    cran <- cran_downloads_fixture()
    result <- serocalculator:::.prepare_download_data(
      cran, NULL, NULL, c("new", "cumulative")
    )

    expect_true("metric" %in% names(result))
    expect_true("downloads" %in% names(result))
    expect_equal(
      levels(result$metric),
      c("New downloads", "Cumulative downloads")
    )
  }
)

test_that(
  desc = ".prepare_download_data() filters by start date",
  code = {
    cran <- cran_downloads_fixture()
    result <- serocalculator:::.prepare_download_data(
      cran, NULL, "2026-01-01", c("new")
    )

    expect_true(all(result$date >= as.Date("2026-01-01")))
  }
)

test_that(
  desc = ".prepare_download_data() includes only requested metrics",
  code = {
    cran <- cran_downloads_fixture()
    result <- serocalculator:::.prepare_download_data(
      cran, NULL, NULL, "new"
    )

    expect_equal(
      levels(result$metric),
      "New downloads"
    )
  }
)

test_that(
  desc = ".plot_downloads() returns ggplot for CRAN only",
  code = {
    cran <- cran_downloads_fixture()
    download_data <- serocalculator:::.prepare_download_data(
      cran, NULL, NULL, c("new", "cumulative")
    )

    p <- serocalculator:::.plot_downloads(
      download_data,
      github = FALSE,
      multi_metric = TRUE,
      title = "Test"
    )

    expect_s3_class(p, "ggplot")
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
    download_data <- serocalculator:::.prepare_download_data(
      cran, gh, NULL, c("new", "cumulative")
    )

    p <- serocalculator:::.plot_downloads(
      download_data,
      github = TRUE,
      multi_metric = TRUE,
      title = "Test"
    )

    expect_s3_class(p, "ggplot")
    p |> vdiffr::expect_doppelganger(
      title = "downloads-cran-github"
    )
  }
)

test_that(
  desc = ".plot_downloads() handles single metric",
  code = {
    cran <- cran_downloads_fixture()
    download_data <- serocalculator:::.prepare_download_data(
      cran, NULL, NULL, "cumulative"
    )

    p <- serocalculator:::.plot_downloads(
      download_data,
      github = FALSE,
      multi_metric = FALSE,
      title = NULL
    )

    expect_s3_class(p, "ggplot")
    p |> vdiffr::expect_doppelganger(
      title = "downloads-cumulative-only"
    )
  }
)

test_that(
  desc = ".check_suggests() passes when packages are available",
  code = {
    expect_no_error(
      serocalculator:::.check_suggests(github = FALSE)
    )
  }
)

test_that(
  desc = "graph_downloads() errors when both metrics are FALSE",
  code = {
    expect_error(
      graph_downloads(new = FALSE, cumulative = FALSE)
    )
  }
)
