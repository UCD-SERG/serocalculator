test_that("`plot_curve_params_one_ab()` produces consistent results", {
  params <-
    data.frame(
      y0 = 10,
      y1 = 10 ^ 4,
      t1 = 9.5,
      alpha = 0.01,
      r = 1,
      iter = 1,
      antigen_iso = "test"
    ) |>
    as_sr_params()


  fig1 <-
    params |>
    plot_curve_params_one_ab(n_points = 10 ^ 5,
                             xlim = c(0, 25),
                             log_y = FALSE)

  fig1 |>
    vdiffr::expect_doppelganger(title = "curve_r1")

})

test_that(
  "`plot_curve_params_one_ab()` works when `iter` does not start at 1",
  {
    # Regression test: previously, layer_function filtered by
    # `iter == cur_iter`, where cur_iter was a row index (1, 2, ...).
    # When iter values didn't start at 1, the filter returned 0 rows,
    # causing `r` to be numeric(0).
    params <-
      data.frame(
        y0 = 10,
        y1 = 10 ^ 4,
        t1 = 9.5,
        alpha = 0.01,
        r = 1,
        iter = 5,
        antigen_iso = "test"
      ) |>
      as_sr_params()

    expect_no_error(
      params |>
        plot_curve_params_one_ab(n_points = 100,
                                 xlim = c(0, 25),
                                 log_y = FALSE)
    )
  }
)
