test_that(
  desc = "stratify_data() produces consistent results",
  code = {
    withr::local_options(width = 80)

    stratified_data =
      stratify_data(
        data = sees_pop_data_pk_100,
        sr_params = typhoid_curves_nostrat_100,
        noise_params = example_noise_params_pk,
        strata_varnames = "catchment",
        curve_strata_varnames = NULL,
        noise_strata_varnames = NULL
      )

    stratified_data |> expect_snapshot()
    stratified_data |> expect_snapshot_value(style = 'serialize')
  })

test_that(
  desc = "stratify_data() produces consistent results with no strata",
  code = {

    library(dplyr)
    library(readr)

    xs_data <-
      sees_pop_data_pk_100

    curve <-
      typhoid_curves_nostrat_100

    noise <-
      example_noise_params_pk

    stratified_data =
      stratify_data(
        data = xs_data,
        sr_params = curve,
        noise_params = noise,
        strata_varnames = NULL,
        curve_strata_varnames = NULL,
        noise_strata_varnames = NULL
      )

    stratified_data |> expect_snapshot_value(style = 'serialize')
  })

test_that(
  desc = "stratify_data() warns about missing data",
  code = {

    library(dplyr)
    library(readr)

    xs_data <-
      sees_pop_data_pk_100 %>%
      filter(row_number() != 1)

    curve <-
      typhoid_curves_nostrat_100 %>%
      slice(1:100, .by = antigen_iso)

    noise <-
      example_noise_params_pk

    stratify_data(
      data = xs_data,
      sr_params = curve,
      noise_params = noise,
      strata_varnames = "catchment",
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL
    ) |>
      expect_warning(regexp = "The number of observations in `data` varies")
  })
