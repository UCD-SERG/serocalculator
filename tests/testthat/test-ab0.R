test_that("`ab0()` produces consistent results", {
  params1 <-
    data.frame(
      y0 = 10,
      y1 = 10^4,
      t1 = 9.5,
      alpha = 0.01,
      r = 1,
      antigen_iso = "test"
    ) |>
    as_sr_params()

  calc1 <-
    ab0(curve_params = params1, t = 9.4)

  expect_snapshot_value(calc1, style = "deparse")

  calc2 <-
    ab0(curve_params = params1, t = 9.6)

  expect_snapshot_value(calc2, style = "deparse")

  params2 <-
    data.frame(
      y0 = 10,
      y1 = 10^4,
      t1 = 9.5,
      alpha = 0.01,
      r = 2,
      antigen_iso = "test"
    ) |>
    as_sr_params()

  calc3 <-
    ab0(curve_params = params2, t = 9.4)

  expect_snapshot_value(calc3, style = "deparse")

  calc4 <-
    ab0(curve_params = params2, t = 9.6)

  expect_snapshot_value(calc4, style = "deparse")
})
