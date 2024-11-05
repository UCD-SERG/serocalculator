

test_that("`est.incidence.by()` warns user when strata is missing", {
  library(dplyr)

  # get pop data
  xs_data <- sees_pop_data_pk_100_standardized

  # get noise data
  noise <- example_noise_params_pk

  # get curve data
  curve <- typhoid_curves_nostrat_100

  expect_warning(
    est.incidence.by(
      pop_data = xs_data,
      curve_params = curve,
      noise_params = noise,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA")
    ),
    class = "strata_empty"
  )
})

test_that(
  "`est.incidence.by()` aborts when elements that don't exactly
          match the columns of `pop_data` are provided",
  {
    xs_data <- sees_pop_data_pk_100_standardized
    noise <- example_noise_params_pk
    curve <- typhoid_curves_nostrat_100

    expect_error(
      object = est.incidence.by(
        strata = c("ag", "catch", "Count"),
        pop_data = xs_data,
        curve_params = curve,
        noise_params = noise %>% filter(Country == "Pakistan"),
        antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
        # num_cores = 8 # Allow for parallel processing to decrease run time
        iterlim = 5 # limit iterations for the purpose of this example
      ),
      class = "missing_var"
    )

  }
)


test_that("est.incidence.by() produces expected results for typhoid data",
          {
            library(dplyr)
            # get pop data
            xs_data <- sees_pop_data_pk_100_standardized
            noise <- example_noise_params_pk
            curve <- typhoid_curves_nostrat_100

            # set start
            start <- .05

            typhoid_results <- est.incidence.by(
              strata = c("catchment"),
              pop_data = xs_data,
              curve_param = curve,
              curve_strata_varnames = NULL,
              noise_strata_varnames = NULL,
              noise_param = noise,
              antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
              # Allow for parallel processing to decrease run time
              num_cores = 1
            )

            expect_snapshot(x = typhoid_results)

            expect_snapshot_value(typhoid_results, style = "deparse")
          })

test_that("`est.incidence.by()` produces expected results
          regardless of whether varnames have been standardized.", {
  library(dplyr)
  xs_data_true <- sees_pop_data_pk_100_standardized
  noise <- example_noise_params_pk
  curves <- typhoid_curves_nostrat_100

  est_true <- est.incidence.by(
    strata = c("catchment"),
    pop_data = xs_data_true,
    curve_params = curves,
    noise_params = noise %>% filter(Country == "Pakistan"),
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL,
    num_cores = 1 # Allow for parallel processing to decrease run time
  )

  xs_data_false <- sees_pop_data_pk_100_nonstandardized

  est_false <- est.incidence.by(
    strata = c("catchment"),
    pop_data = xs_data_false,
    curve_params = curves,
    noise_params = noise %>% filter(Country == "Pakistan"),
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    num_cores = 1 # Allow for parallel processing to decrease run time
  )

  expect_equal(est_true, est_false)
})
