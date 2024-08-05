test_that("`as_noise_params()` produces an error when non-noise data is provided", {
  library(magrittr)
  expect_error(
    object = noise_data <-
      "https://osf.io/download//n6cp3/" %>% # pop data
      readr::read_rds() %>%
      as_noise_params(),
    class = "not noise_params"
  )
})

test_that("`as_noise_params()` produces expected results", {
  library(dplyr)
  test_data <- "https://osf.io/download//hqy4v/" %>% # noise data
    readr::read_rds() %>%
    slice_head(n = 100) %>%
    as_noise_params()

  expect_snapshot(test_data)

  expect_snapshot_value(
    x = test_data,
    style = "serialize"
  )


})
