test_that("stratify_data() produces consistent results", {

  library(dplyr)
  library(readr)

  xs_data <-
    sees_pop_data_pk_100 %>%
    filter(catchment == "aku")

  curve <-
    typhoid_curves_nostrat_100 %>%
    slice(1:100, .by = antigen_iso)

  noise <-
    example_noise_params_pk

  stratified_data =
    stratify_data(
      data = xs_data,
      curve_params = curve,
      noise_params = noise,
      strata_varnames = "catchment",
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL
    )

  expect_snapshot(stratified_data)
})

test_that("stratify_data() warns about missing data", {

  library(dplyr)
  library(readr)

  xs_data <-
    sees_pop_data_pk_100 %>%
    filter(catchment == "kgh")

  curve <-
    typhoid_curves_nostrat_100 %>%
    slice(1:100, .by = antigen_iso)

  noise <-
    example_noise_params_pk

  stratify_data(
    data = xs_data,
    curve_params = curve,
    noise_params = noise,
    strata_varnames = "catchment",
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL
  ) |>
    expect_warning(regexp = "The number of observations in `data` varies")
})
