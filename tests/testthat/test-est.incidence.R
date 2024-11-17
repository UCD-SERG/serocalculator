
test_that("est.incidence() produces expected results for typhoid data", {
  typhoid_results <- est.incidence(
    pop_data = sees_pop_data_pk_100,
    curve_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  expect_snapshot(x = summary(typhoid_results))

  expect_snapshot_value(typhoid_results, style = "deparse", tolerance = 1e-4)
})

test_that(
  "`est.incidence()` produces consistent results
          regardless of whether data colnames are standardized.",
  {
    est_true <- est.incidence(
      pop_data = sees_pop_data_pk_100,
      curve_param = typhoid_curves_nostrat_100,
      noise_param = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA")
    )

    est_false <- est.incidence(
      pop_data = sees_pop_data_pk_100_old_names,
      curve_param = typhoid_curves_nostrat_100,
      noise_param = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA")
    )

    expect_equal(est_true, est_false)
  }
)
