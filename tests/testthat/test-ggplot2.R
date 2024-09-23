test_that("consistent theme", {
  ggplot2::theme_get() |>
  expect_snapshot()
})
