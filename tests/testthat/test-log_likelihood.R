test_that("`log_likelihood()` gives consistent results", {

  # Calculate log-likelihood
  ll_ag <- log_likelihood(
    pop_data = sees_pop_data_pk_100,
    curve_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    lambda = 0.1
  )

  expect_snapshot_value(ll_ag, style = "deparse")

})
