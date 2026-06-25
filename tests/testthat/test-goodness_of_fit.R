test_that("gof_summary computes valid metrics with clustered fit", {
  # Fit a model with clustering to ensure parameters are stored
  xs_data <- sees_pop_data_pk_100
  curve <- typhoid_curves_nostrat_100 %>%
    dplyr::filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))
  noise <- example_noise_params_pk

  est <- est_seroincidence(
    pop_data = xs_data,
    sr_params = curve,
    noise_params = noise,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = "cluster",
    iterlim = 5
  )

  # Compute GOF summary
  gof <- gof_summary(est, xs_data, verbose = FALSE)

  # Check structure
  expect_s3_class(gof, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(gof), 1)

  # Check required columns
  expect_true("log_likelihood" %in% names(gof))
  expect_true("aic" %in% names(gof))
  expect_true("bic" %in% names(gof))
  expect_true("n_observations" %in% names(gof))
  expect_true("n_parameters" %in% names(gof))
})

test_that("posterior_predictive_check requires parameters stored", {
  # Fit model WITHOUT clustering - parameters won't be stored
  xs_data <- sees_pop_data_pk_100
  curve <- typhoid_curves_nostrat_100 %>%
    dplyr::filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))
  noise <- example_noise_params_pk

  est <- est_seroincidence(
    pop_data = xs_data,
    sr_params = curve,
    noise_params = noise,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    iterlim = 5
  )

  # Should error because parameters not stored
  expect_error(
    posterior_predictive_check(est, xs_data, n_sim = 2, verbose = FALSE),
    "Cannot perform posterior predictive check"
  )
})

test_that("gof_summary validates inputs", {
  # Not a seroincidence object
  expect_error(
    gof_summary(list(), sees_pop_data_pk_100),
    "`object` must be a seroincidence object"
  )
})

test_that("posterior_predictive_check validates inputs", {
  # Not a seroincidence object
  expect_error(
    posterior_predictive_check(list(), sees_pop_data_pk_100),
    "`object` must be a seroincidence object"
  )
})

test_that("calculate_residuals validates inputs", {
  # Not a seroincidence object
  expect_error(
    calculate_residuals(list(), sees_pop_data_pk_100),
    "`object` must be a seroincidence object"
  )
})
