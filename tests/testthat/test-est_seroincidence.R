test_that("results are as expected for typhoid data", {
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
  "results are consistent
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

    # Remove pop_data_name attribute before comparison
    # since it will differ based on input variable name
    attr(est_true, "pop_data_name") <- NULL
    attr(est_false, "pop_data_name") <- NULL

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

test_that(
  "lifecycle warning works as expected",
  code = {
    lifecycle_test <- est.incidence(
      pop_data = sees_pop_data_pk_100,
      sr_param = typhoid_curves_nostrat_100,
      noise_param = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA")
    ) |>
      expect_warning()
  }
)

test_that("summary includes noise_params and sr_params metadata", {
  est1 <- est_seroincidence(
    pop_data = sees_pop_data_pk_100,
    sr_param = typhoid_curves_nostrat_100 |>
      dplyr::filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")),
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  summ <- summary(est1, show_full_input = TRUE)

  # Check that noise parameter columns exist with antigen names
  expect_true("measurement.noise.HlyE_IgA" %in% names(summ))
  expect_true("measurement.noise.HlyE_IgG" %in% names(summ))
  expect_true("biological.noise.HlyE_IgA" %in% names(summ))
  expect_true("biological.noise.HlyE_IgG" %in% names(summ))

  # Check that metadata columns exist and have correct values
  expect_true("n.seroresponse.params" %in% names(summ))
  expect_equal(summ$n.seroresponse.params, 200)

  expect_true("seroresponse.params.stratified" %in% names(summ))
  expect_false(summ$seroresponse.params.stratified)

  # Check that object name columns exist with correct new names
  expect_true("pop_data" %in% names(summ))
  expect_true("sr_params" %in% names(summ))
  expect_true("noise_params" %in% names(summ))
  expect_equal(summ$noise_params, "example_noise_params_pk")
})
