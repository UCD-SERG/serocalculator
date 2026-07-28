test_that("`pathogen_decay_curve()` returns `b0` at `t = 0`", {
  b0 <- 1
  expect_equal(
    pathogen_decay_curve(t = 0, b0 = b0),
    b0
  )
})

test_that("`pathogen_decay_curve()` matches a hand-computed value", {
  t <- 5
  y0 <- 1
  b0 <- 1
  mu_b <- 0.1
  mu_y <- 0.2
  gamma <- 0.01

  fraction <- gamma * y0 * (exp(mu_y * t) - exp(mu_b * t)) / (mu_y - mu_b)
  expected <- max(0, (b0 * exp(mu_b * t)) - fraction)

  expect_equal(
    pathogen_decay_curve(
      t = t, y0 = y0, b0 = b0, mu_b = mu_b, mu_y = mu_y, gamma = gamma
    ),
    expected
  )
})

test_that("`pathogen_decay_curve()` is floored at zero", {
  result <- pathogen_decay_curve(
    t = 1000, y0 = 100, b0 = 1, mu_b = 0.01, mu_y = 1, gamma = 10
  )
  expect_equal(result, 0)
})

test_that("`pathogen_decay_curve()` uses its documented defaults", {
  expect_equal(pathogen_decay_curve(t = 5), pathogen_decay_curve(
    t = 5,
    y0 = 0.74916052,
    b0 = 1,
    mu_b = 0.18432798,
    mu_y = 0.36853621,
    gamma = 0.0013040664
  ))
})

test_that("`pathogen_decay_curve()` errors on negative parameters", {
  expect_error(pathogen_decay_curve(t = 1, y0 = -1), class = "rlang_error")
  expect_error(pathogen_decay_curve(t = 1, gamma = -1), class = "rlang_error")
})
