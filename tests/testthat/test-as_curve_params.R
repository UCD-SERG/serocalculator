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

test_that("`as_curve_params()` produces an error when `data` is not a data.frame",
          {
            library(magrittr)
            expect_error(object =
                           "https://osf.io/download//n6cp3/" %>% # pop data
                           as_curve_params(), class = "not data.frame")
          })

test_that("`as_curve_params()` produces expected results", {
  library(dplyr)
  test_data <- "https://osf.io/download/rtw5k/" %>% # curve data
    readr::read_rds() %>%
    slice_head(n = 100) %>%
    as_curve_params()

  expect_snapshot(test_data)

  expect_snapshot_value(x = test_data, style = "serialize")

  test_data %>% ssdtools:::expect_snapshot_data(name = "curve-data")


})
