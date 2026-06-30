test_that("`y1f()` matches a hand-computed value", {
  y1 <- y1f(y0 = 1, mu_y = 1, t1 = 10)
  expect_equal(y1, 1 * exp(1 * 10))
})

test_that("`y1f()` derives `t1` from `t1f()` when not supplied", {
  y1_explicit <- y1f(y0 = 1, mu_y = 0.2, t1 = t1f(y0 = 1, mu_y = 0.2))
  y1_implicit <- y1f(y0 = 1, mu_y = 0.2)
  expect_equal(y1_implicit, y1_explicit)
})

test_that("`y1f()` uses its documented defaults", {
  expect_equal(y1f(), y1f(y0 = 0.74916052, mu_y = 0.36853621))
})

test_that("`y1f()` errors on negative parameters", {
  expect_error(y1f(y0 = -1, mu_y = 1, t1 = 1), class = "rlang_error")
  expect_error(y1f(y0 = 1, mu_y = -1, t1 = 1), class = "rlang_error")
})
