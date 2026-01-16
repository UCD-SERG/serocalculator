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

  summ <- summary(est1)

  # Check that noise_params attribute exists and has correct structure
  expect_true(!is.null(attr(summ, "noise_params")))
  noise_params <- attr(summ, "noise_params")
  expect_s3_class(noise_params, "tbl_df")
  expect_true(all(c("antigen_iso", "eps", "nu") %in% names(noise_params)))
  expect_equal(nrow(noise_params), 2)

  # Check that n_sr_params attribute exists and is correct
  expect_true(!is.null(attr(summ, "n_sr_params")))
  expect_equal(attr(summ, "n_sr_params"), 200)

  # Check that n_pop_data attribute exists and is correct
  expect_true(!is.null(attr(summ, "n_pop_data")))
  expect_equal(attr(summ, "n_pop_data"), 200)

  # Check that sr_params_stratified attribute exists and is FALSE
  expect_true(!is.null(attr(summ, "sr_params_stratified")))
  expect_false(attr(summ, "sr_params_stratified"))
})

