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

test_that("multi-biomarker log_likelihood() sums the per-biomarker values", {

  # `log_likelihood()` combines biomarkers by summing their marginal
  # log-likelihoods (an independence/composite likelihood), rather than
  # integrating a shared latent infection time as the methodology article
  # describes -- see issue #637. This test locks in that identity for the
  # default method, so a change to the underlying model doesn't silently
  # alter it without a deliberate update to this test. The shared-latent-
  # time model is available as `method = "joint"` (#646); see
  # test-f_dev_joint.R.

  ll_iga <- log_likelihood(
    pop_data = sees_pop_data_pk_100,
    curve_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = "HlyE_IgA",
    lambda = 0.1
  )

  ll_igg <- log_likelihood(
    pop_data = sees_pop_data_pk_100,
    curve_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = "HlyE_IgG",
    lambda = 0.1
  )

  ll_both <- log_likelihood(
    pop_data = sees_pop_data_pk_100,
    curve_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    lambda = 0.1
  )

  expect_equal(ll_iga + ll_igg, ll_both)

})
