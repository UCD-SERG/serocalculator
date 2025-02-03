test_that("`as_noise_params()` produces an error
          when non-noise data is provided", {
            library(magrittr)
            expect_error(
              object = noise_data <-
                serocalculator_example("example_pop_data.csv") %>% # pop data
                read.csv() %>%
                as_noise_params(),
              class = "not noise_params"
            )
          })

test_that("`as_noise_params()` produces expected results", {
  library(dplyr)
  test_data <- serocalculator_example("example_noise_params.csv") %>% # noise data
    read.csv(row.names = 1) %>%
    as_noise_params()

  expect_snapshot(test_data)

  expect_snapshot_value(
    x = test_data,
    style = "serialize"
  )
})

test_that("`as_noise_params()` produces error when
          wrong antigen_iso is provided", {
            library(dplyr)

            expect_error(
              object = serocalculator_example("example_noise_params.csv") %>% # noise data
                read.csv() %>%
                as_noise_params(antigen_iso = "HlyE_IgB"), # antigen_iso doesn't match
              class = "missing-antigen"
            )
          })

test_that("`as_noise_params()` produces error when
          non-data frame is provided", {
            library(dplyr)

            expect_error(
              object = "a string sample" %>% # random string
                as_noise_params(),
              class = "not data.frame"
            )
          })
