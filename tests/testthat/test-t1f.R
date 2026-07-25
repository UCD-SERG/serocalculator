test_that("`t1f()` matches a hand-computed value", {
  t1 <- t1f(
    y0 = 1,
    b0 = 1,
    mu_b = 0.1,
    mu_y = 0.2,
    gamma = 0.01
  )

  expected <- log(1 + (0.2 - 0.1) * 1 / (0.01 * 1)) / (0.2 - 0.1)

  expect_equal(t1, expected)
})

test_that("`t1f()` uses its documented defaults", {
  expect_equal(t1f(), t1f(
    y0 = 0.74916052,
    b0 = 1,
    mu_b = 0.18432798,
    mu_y = 0.36853621,
    gamma = 0.0013040664
  ))
})

test_that("`t1f()` errors when `mu_y` equals `mu_b`", {
  expect_error(
    t1f(mu_y = 0.2, mu_b = 0.2),
    class = "rlang_error"
  )
})

test_that("`t1f()` errors on negative parameters", {
  expect_error(t1f(y0 = -1), class = "rlang_error")
  expect_error(t1f(b0 = -1), class = "rlang_error")
})

test_that("`t1f()` errors when the log argument is non-positive", {
  expect_error(
    t1f(y0 = 1, b0 = 1, mu_b = 0.5, mu_y = 0.1, gamma = 0.01),
    class = "rlang_error"
  )
})
