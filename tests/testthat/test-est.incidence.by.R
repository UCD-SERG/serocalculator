



test_that("`est.incidence.by()` warns user when strata is missing", {
  expect_warning(
    est.incidence.by(
      pop_data = sees_pop_data_pk_100_standardized,
      curve_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA")
    ),
    class = "strata_empty"
  )
})

test_that(
  "`est.incidence.by()` aborts when elements that don't exactly
          match the columns of `pop_data` are provided",
  {
    expect_error(
      object = est.incidence.by(
        strata = c("ag", "catch", "Count"),
        pop_data = sees_pop_data_pk_100_standardized,
        curve_params = typhoid_curves_nostrat_100,
        noise_params = example_noise_params_pk,
        antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
        num_cores = 8,
        # Allow for parallel processing to decrease run time
        iterlim = 5 # limit iterations for the purpose of this example
      ),
      class = "missing_var"
    )

  }
)


test_that("est.incidence.by() produces expected results for typhoid data",
          {
            typhoid_results <- est.incidence.by(
              strata = c("catchment"),
              pop_data = sees_pop_data_pk_100_standardized,
              curve_param = typhoid_curves_nostrat_100,
              curve_strata_varnames = NULL,
              noise_strata_varnames = NULL,
              noise_param = example_noise_params_pk,
              antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
              # Allow for parallel processing to decrease run time
              num_cores = 1
            )

            expect_snapshot(x = typhoid_results)

            expect_snapshot_value(typhoid_results, style = "deparse")
          })

test_that(
  "`est.incidence.by()` produces expected results
          regardless of whether varnames have been standardized.",
  {
    est_true <- est.incidence.by(
      strata = c("catchment"),
      pop_data = sees_pop_data_pk_100_standardized,
      curve_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      num_cores = 1 # Allow for parallel processing to decrease run time
    )

    est_false <- est.incidence.by(
      strata = c("catchment"),
      pop_data = sees_pop_data_pk_100_nonstandardized,
      curve_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      num_cores = 1 # Allow for parallel processing to decrease run time
    )

    expect_equal(est_true, est_false)
  }
)
