test_that("stratify_data() produces consistent results", {

  library(dplyr)
  library(readr)

  xs_data <-
    read_rds("https://osf.io/download//n6cp3/")  |>
    as_pop_data() |>
    filter(Country == "Pakistan")

  curve <-
    load_curve_params("https://osf.io/download/rtw5k/") |>
    filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) |>
    slice(1:100, .by = antigen_iso) # Reduce dataset for the purposes of this example

  noise <-
    load_noise_params("https://osf.io/download//hqy4v/") |>
    filter(Country == "Pakistan")

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
    read_rds("https://osf.io/download//n6cp3/")  |>
    as_pop_data() |>
    filter(Country == "Nepal")

  curve <-
    load_curve_params("https://osf.io/download/rtw5k/") |>
    filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) |>
    slice(1:100, .by = antigen_iso) # Reduce dataset for the purposes of this example

  noise <-
    load_noise_params("https://osf.io/download//hqy4v/") |>
    filter(Country == "Nepal")

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
