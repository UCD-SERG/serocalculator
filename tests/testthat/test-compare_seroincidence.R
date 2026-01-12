test_that("compare_seroincidence works with two seroincidence objects", {
  withr::local_package("dplyr")

  # Create two separate estimates
  est1 <- est_seroincidence(
    pop_data = sees_pop_data_pk_100 |> filter(catchment == "kgh"),
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  est2 <- est_seroincidence(
    pop_data = sees_pop_data_pk_100 |> filter(catchment == "aku"),
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  result <- compare_seroincidence(est1, est2)

  # Check that result is an htest object with correct structure
  expect_s3_class(result, "htest")
  expect_snapshot(result)
})

test_that("compare_seroincidence works with seroincidence.by object", {
  withr::local_package("dplyr")

  # Create stratified estimates
  est_by <- est_seroincidence_by(
    strata = "catchment",
    pop_data = sees_pop_data_pk_100,
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL
  )

  result <- compare_seroincidence(est_by)

  # Check that result is a tibble with correct class and structure
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "comparison.seroincidence.by")
  
  # Use snapshot to validate structure and values
  expect_snapshot_value(result, style = "serialize", tolerance = 1e-4)
})

test_that("compare_seroincidence works with multiple strata variables", {
  withr::local_package("dplyr")

  # Create stratified estimates with multiple variables
  est_by <- est_seroincidence_by(
    strata = c("catchment", "ageCat"),
    pop_data = sees_pop_data_pk_100,
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL
  )

  result <- compare_seroincidence(est_by)

  # Validate structure via snapshot
  expect_snapshot_value(result, style = "serialize", tolerance = 1e-4)
})

test_that("compare_seroincidence errors appropriately", {
  withr::local_package("dplyr")

  est1 <- est_seroincidence(
    pop_data = sees_pop_data_pk_100 |> filter(catchment == "kgh"),
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  # Missing y parameter
  expect_error(
    compare_seroincidence(est1),
    class = "rlang_error"
  )

  # Wrong class for y
  expect_error(
    compare_seroincidence(est1, y = "not a seroincidence object"),
    class = "rlang_error"
  )

  # Wrong class for x
  expect_error(
    compare_seroincidence("not a seroincidence object"),
    class = "rlang_error"
  )

  # Fewer than 2 strata
  est_by_single <- est_seroincidence_by(
    strata = "catchment",
    pop_data = sees_pop_data_pk_100 |> filter(catchment == "kgh"),
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL
  )

  expect_error(
    compare_seroincidence(est_by_single),
    class = "rlang_error"
  )
})

test_that("compare_seroincidence respects coverage parameter", {
  withr::local_package("dplyr")

  est1 <- est_seroincidence(
    pop_data = sees_pop_data_pk_100 |> filter(catchment == "kgh"),
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  est2 <- est_seroincidence(
    pop_data = sees_pop_data_pk_100 |> filter(catchment == "aku"),
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  result_95 <- compare_seroincidence(est1, est2, coverage = 0.95)
  result_90 <- compare_seroincidence(est1, est2, coverage = 0.90)

  # Check that confidence levels are set correctly
  expect_equal(attr(result_95$conf.int, "conf.level"), 0.95)
  expect_equal(attr(result_90$conf.int, "conf.level"), 0.90)

  # Check that 90% CI is narrower than 95% CI
  ci_width_95 <- result_95$conf.int[2] - result_95$conf.int[1]
  ci_width_90 <- result_90$conf.int[2] - result_90$conf.int[1]
  expect_true(ci_width_90 < ci_width_95)
})

test_that(
  "compare_seroincidence warns when y is provided for seroincidence.by",
  {
    withr::local_package("dplyr")

    est_by <- est_seroincidence_by(
      strata = "catchment",
      pop_data = sees_pop_data_pk_100,
      sr_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL
    )

    est1 <- est_seroincidence(
      pop_data = sees_pop_data_pk_100 |> filter(catchment == "kgh"),
      sr_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA")
    )

    expect_warning(
      compare_seroincidence(est_by, y = est1),
      class = "rlang_warning"
    )
  }
)
