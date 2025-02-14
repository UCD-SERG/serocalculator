test_that("results are consistent without units", {
  params <- typhoid_curves_nostrat_100[1, ]
  results1 <- tibble::tibble(
    t = seq(0, 100, by = 5),
    y = ab_5p(
      t = t,
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
    ggplot2::aes(x = t, y = y) +
    ggplot2::geom_point() +
    ggplot2::geom_line()

  fig1 |> vdiffr::expect_doppelganger(title = "response_curve")

})
