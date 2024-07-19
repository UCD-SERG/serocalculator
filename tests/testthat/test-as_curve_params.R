test_that("`as_curve_params()` produces an error when non-curve data is provided", {
  library(magrittr)
  expect_error(
    object = curve_data <-
      "https://osf.io/download//n6cp3/" %>% # pop data
      readr::read_rds() %>%
      as_curve_params(),
    class = "not curve_params"
  )
})


test_that("`as_curve_params()` produces expected results", {
  test_data <- "https://osf.io/download/rtw5k/" %>% # curve data
    readr::read_rds() %>%
    as_curve_params()

  expect_snapshot_value(
    x = test_data,
    style = "serialize"
  )
})