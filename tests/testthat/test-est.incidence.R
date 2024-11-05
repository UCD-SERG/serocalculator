test_that(
  "est.incidence() produces expected results for typhoid data",
  {

    typhoid_results <- est.incidence(
      pop_data = sees_pop_data_pk_100_standardized,
      curve_param = typhoid_curves_nostrat_100,
      noise_param = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA")
    )

    expect_snapshot(x = summary(typhoid_results))

    expect_snapshot_value(
      typhoid_results, style = "deparse"
    )
  }
)

test_that("`est.incidence()` produces expected results", {

  est_true <- est.incidence(
    pop_data = sees_pop_data_pk_100_standardized,
    curve_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  est_false <- est.incidence(
    pop_data = sees_pop_data_pk_100_nonstandardized,
    curve_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  expect_equal(est_true, est_false)
})
