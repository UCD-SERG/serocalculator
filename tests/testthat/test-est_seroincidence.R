test_that("est_seroincidence() produces expected results for typhoid data", {
  typhoid_results <- est_seroincidence(
    pop_data = sees_pop_data_pk_100,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  expect_snapshot(x = summary(typhoid_results, coverage = .95))

  expect_snapshot_value(typhoid_results, style = "deparse", tolerance = 1e-4)

})

test_that(
  "`estimate_scr()` produces consistent results
          regardless of whether data colnames are standardized.",
  {
    est_true <- est_seroincidence(
      pop_data = sees_pop_data_pk_100,
      sr_param = typhoid_curves_nostrat_100,
      noise_param = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA")
    )

    est_false <- est_seroincidence(
      pop_data = sees_pop_data_pk_100_old_names,
      sr_param = typhoid_curves_nostrat_100,
      noise_param = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA")
    )

    expect_equal(est_true, est_false)
  }
)

test_that(
  "verbose output is consistent",
  code = {
    skip_on_os("mac")
    withr::local_options(
                         list(
                              width = 80,
                              digits = 8))

    est_seroincidence(
      pop_data = sees_pop_data_pk_100,
      sr_param = typhoid_curves_nostrat_100,
      noise_param = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      verbose = TRUE
    ) |>
      expect_snapshot()
  }
)
