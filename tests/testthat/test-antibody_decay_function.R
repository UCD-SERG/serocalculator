test_that("`antibody_decay_curve()` returns `y0` at `t = 0`", {
  y0 <- 0.74916052
  expect_equal(antibody_decay_curve(t = 0, y0 = y0), y0)
})

test_that("`antibody_decay_curve()` follows phase-1 exponential growth before `t1`", {
  y0 <- 1
  mu_y <- 0.2
  t1 <- t1f(y0 = y0, mu_y = mu_y, b0 = 1, mu_b = 0.1, gamma = 0.01)
  t_before <- t1 / 2

  expect_equal(
    antibody_decay_curve(
      t = t_before, y0 = y0, b0 = 1, mu_b = 0.1, mu_y = mu_y, gamma = 0.01
    ),
    y0 * exp(mu_y * t_before)
  )
})

test_that("`antibody_decay_curve()` matches `y1f()` at `t1`", {
  y0 <- 1
  b0 <- 1
  mu_b <- 0.1
  mu_y <- 0.2
  gamma <- 0.01
  t1 <- t1f(y0 = y0, b0 = b0, mu_b = mu_b, mu_y = mu_y, gamma = gamma)
  y1 <- y1f(y0 = y0, mu_y = mu_y, t1 = t1)

  expect_equal(
    antibody_decay_curve(
      t = t1, y0 = y0, b0 = b0, mu_b = mu_b, mu_y = mu_y, gamma = gamma
    ),
    y1
  )
})

test_that("`antibody_decay_curve()` handles the `rho = 1` special case", {
  y0 <- 1
  b0 <- 1
  mu_b <- 0.1
  mu_y <- 0.2
  gamma <- 0.01
  alpha <- 0.001
  t1 <- t1f(y0 = y0, b0 = b0, mu_b = mu_b, mu_y = mu_y, gamma = gamma)
  y1 <- y1f(y0 = y0, mu_y = mu_y, t1 = t1)
  t_after <- t1 + 5

  expect_equal(
    antibody_decay_curve(
      t = t_after, y0 = y0, b0 = b0, mu_b = mu_b, mu_y = mu_y,
      gamma = gamma, alpha = alpha, rho = 1
    ),
    y1 * exp(-alpha * (t_after - t1))
  )
})

test_that("`antibody_decay_curve()` uses its documented defaults", {
  expect_equal(antibody_decay_curve(t = 5), antibody_decay_curve(
    t = 5,
    y0 = 0.74916052,
    b0 = 1,
    mu_b = 0.18432798,
    mu_y = 0.36853621,
    gamma = 0.0013040664,
    alpha = 0.00002192627,
    rho = 2
  ))
})

test_that("`antibody_decay_curve()` errors on negative parameters", {
  expect_error(antibody_decay_curve(t = 1, y0 = -1), class = "rlang_error")
  expect_error(antibody_decay_curve(t = 1, alpha = -1), class = "rlang_error")
})
