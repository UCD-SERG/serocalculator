test_that(
  desc = "results are consistent without units",
  code = {

    params <- typhoid_curves_nostrat_100[1, ]
    results1 <- tibble::tibble(
      times = seq(0, 100, by = 5),
      y = ab_5p(
        t = times,
        y0 = params$y0,
        y1 = params$y1,
        t1 = params$t1,
        alpha = params$alpha,
        shape = params$r)
    )

    results1 |>
      expect_snapshot_value(style = "deparse")

    fig1 <-
      results1 |>
      ggplot2::ggplot() +
      ggplot2::aes(x = times, y = y) +
      ggplot2::geom_point() +
      ggplot2::geom_line()

    fig1 |> vdiffr::expect_doppelganger(title = "response_curve")

  }
)

test_that("results are consistent with units", {
  params <- typhoid_curves_nostrat_100[1, ]
  results1 <- tibble::tibble(
    t = seq(0, 100, by = 5) |> units::as_units("years"),
    y = ab_5p(
      t = t,
      y0 = params$y0,
      y1 = params$y1,
      t1 = params$t1 |> units::as_units("days"),
      alpha = params$alpha |> units::as_units("1/years"),
      shape = params$r)
  )

  results1 |>
    expect_snapshot_value(style = "serialize")

  library(units)
  fig2 <-
    results1 |>
    ggplot2::ggplot() +
    ggplot2::aes(x = t, y = y) +
    ggplot2::geom_point() +
    ggplot2::geom_line()

  fig2 |> vdiffr::expect_doppelganger(title = "response_curve-units")

})

test_that(
  desc = "results are consistent with multiple curves",
  code = {

    params <- typhoid_curves_nostrat_100[1:2, ]
    results1 <- tibble::tibble(
      times = c(50, 50),
      y = ab_5p(
        t = times,
        y0 = params$y0,
        y1 = params$y1,
        t1 = params$t1,
        alpha = params$alpha,
        shape = params$r)
    )

    results1 |>
      expect_snapshot_value(style = "deparse")


  }
)

test_that(
  desc = "infinite time since seroconversion produces y = 0",
  code = {

    params <- typhoid_curves_nostrat_100[1, ]
    results1 <- tibble::tibble(
      times = Inf,
      y = ab_5p(
        t = times,
        y0 = params$y0,
        y1 = params$y1,
        t1 = params$t1,
        alpha = params$alpha,
        shape = params$r)
    )

      expect_equal(object = results1$y, expected = 0)

  }
)
