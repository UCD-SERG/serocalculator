test_that("`ab0()` produces consistent results", {
  params1 <-
    data.frame(
      y0 = 10,
      y1 = 10 ^ 4,
      t1 = 9.5,
      alpha = 0.01,
      r = 1,
      antigen_iso = "test"
    ) |>
    as_curve_params()

  calc1 =
    ab0(curve_params = params1, t = 9.4)

  expect_snapshot(calc1)

  calc2 =
    ab0(curve_params = params1, t = 9.6)

  expect_snapshot(calc2)

  params2 <-
    data.frame(
      y0 = 10,
      y1 = 10^4,
      t1 = 9.5,
      alpha = 0.01,
      r = 2,
      antigen_iso = "test"
    ) |>
    as_curve_params()

  calc3 =
    ab0(curve_params = params2, t = 9.4)

  expect_snapshot(calc3)

  calc4 =
    ab0(curve_params = params2, t = 9.6)

  expect_snapshot(calc4)

})
