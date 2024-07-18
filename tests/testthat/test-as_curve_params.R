test_that("`as_curve_params()` produces an error when non-curve data is provided", {
  library(magrittr)
  expect_error(
    object = curve_data <-
      "https://osf.io/download//n6cp3/" %>% # pop data
      readr::read_rds() %>%
      as_curve_params(),
    regexp = "Please provide curve data",
    fixed = TRUE
  )
})

test_that("`as_curve_params()` produces expected results", {
  expect_snapshot(
    curve_data <-
      "https://osf.io/download/rtw5k/" %>% # curve data
      readr::read_rds() %>%
      as_curve_params()
  )
})
