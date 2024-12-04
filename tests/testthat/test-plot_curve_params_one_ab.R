test_that("`plot_curve_params_one_ab()` produces consistent results", {
  params <-
    data.frame(
      y0 = 10,
      y1 = 10 ^ 4,
      t1 = 9.5,
      alpha = 0.01,
      r = 1,
      antigen_iso = "test"
    ) |>
    as_curve_params()


  fig1 <-
    params |>
    plot_curve_params_one_ab(n_points = 10 ^ 5,
                             xlim = c(0, 25),
                             log_y = FALSE)

  fig1 |>
    vdiffr::expect_doppelganger(title = "curve_r1")

})
