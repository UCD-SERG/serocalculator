test_that("`.validate_decay_params()` passes silently for valid input", {
  expect_no_error(
    .validate_decay_params(y0 = 1, b0 = 1, mu_b = 0.1, mu_y = 0.2, gamma = 0.01)
  )
})

test_that("`.validate_decay_params()` errors on a negative parameter", {
  expect_error(
    .validate_decay_params(y0 = -1, b0 = 1, mu_b = 0.1, mu_y = 0.2, gamma = 0.01),
    class = "rlang_error"
  )
})

test_that("`.validate_decay_params()` errors when `mu_y` equals `mu_b`", {
  expect_error(
    .validate_decay_params(y0 = 1, b0 = 1, mu_b = 0.2, mu_y = 0.2, gamma = 0.01),
    class = "rlang_error"
  )
})
