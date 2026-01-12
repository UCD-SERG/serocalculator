test_that("compare_seroincidence works with two seroincidence objects", {
  withr::local_package("dplyr")

  # Create two separate estimates
  est1 <- est_seroincidence(
    pop_data = sees_pop_data_pk_100 |> filter(catchment == "c1"),
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  est2 <- est_seroincidence(
    pop_data = sees_pop_data_pk_100 |> filter(catchment == "c2"),
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  result <- compare_seroincidence(est1, est2)

  # Check that result is an htest object
  expect_s3_class(result, "htest")

  # Check that required components exist
  expect_true("statistic" %in% names(result))
  expect_true("p.value" %in% names(result))
  expect_true("estimate" %in% names(result))
  expect_true("conf.int" %in% names(result))
  expect_true("null.value" %in% names(result))
  expect_true("alternative" %in% names(result))
  expect_true("method" %in% names(result))

  # Check that p-value is valid
  expect_true(result$p.value >= 0 && result$p.value <= 1)

  # Check that estimates are positive
  expect_true(all(result$estimate > 0))

  # Check that confidence interval has correct attributes
  expect_equal(attr(result$conf.int, "conf.level"), 0.95)
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

  # Check that result is a tibble
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "comparison.seroincidence.by")

  # Check that required columns exist
  expect_true("Stratum_1" %in% names(result))
  expect_true("Stratum_2" %in% names(result))
  expect_true("incidence.rate.1" %in% names(result))
  expect_true("incidence.rate.2" %in% names(result))
  expect_true("difference" %in% names(result))
  expect_true("SE" %in% names(result))
  expect_true("z.statistic" %in% names(result))
  expect_true("p.value" %in% names(result))
  expect_true("CI.lwr" %in% names(result))
  expect_true("CI.upr" %in% names(result))

  # Check that p-values are valid
  expect_true(all(result$p.value >= 0 & result$p.value <= 1))

  # Check that all incidence rates are positive
  expect_true(all(result$incidence.rate.1 > 0))
  expect_true(all(result$incidence.rate.2 > 0))

  # Check metadata
  expect_equal(attr(result, "coverage"), 0.95)
  expect_true("strata_vars" %in% names(attributes(result)))
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

  # Check that result is a tibble
  expect_s3_class(result, "tbl_df")

  # Check that stratum variable columns exist
  expect_true("catchment.1" %in% names(result))
  expect_true("catchment.2" %in% names(result))
  expect_true("ageCat.1" %in% names(result))
  expect_true("ageCat.2" %in% names(result))

  # Check that we have the correct number of comparisons
  n_strata <- length(unique(est_by))
  expected_comparisons <- choose(n_strata, 2)
  expect_equal(nrow(result), expected_comparisons)
})

test_that("compare_seroincidence errors when y is missing for seroincidence object", {
  withr::local_package("dplyr")

  est1 <- est_seroincidence(
    pop_data = sees_pop_data_pk_100 |> filter(catchment == "c1"),
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  expect_error(
    compare_seroincidence(est1),
    class = "rlang_error"
  )
})

test_that("compare_seroincidence errors when y is wrong class", {
  withr::local_package("dplyr")

  est1 <- est_seroincidence(
    pop_data = sees_pop_data_pk_100 |> filter(catchment == "c1"),
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  expect_error(
    compare_seroincidence(est1, y = "not a seroincidence object"),
    class = "rlang_error"
  )
})

test_that("compare_seroincidence errors when x is wrong class", {
  expect_error(
    compare_seroincidence("not a seroincidence object"),
    class = "rlang_error"
  )
})

test_that("compare_seroincidence errors with fewer than 2 strata", {
  withr::local_package("dplyr")

  # Create an estimate with only one stratum by filtering
  est_by <- est_seroincidence_by(
    strata = "catchment",
    pop_data = sees_pop_data_pk_100 |> filter(catchment == "c1"),
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL
  )

  expect_error(
    compare_seroincidence(est_by),
    class = "rlang_error"
  )
})

test_that("compare_seroincidence respects coverage parameter", {
  withr::local_package("dplyr")

  est1 <- est_seroincidence(
    pop_data = sees_pop_data_pk_100 |> filter(catchment == "c1"),
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  est2 <- est_seroincidence(
    pop_data = sees_pop_data_pk_100 |> filter(catchment == "c2"),
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

test_that("compare_seroincidence warns when y is provided for seroincidence.by", {
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
    pop_data = sees_pop_data_pk_100 |> filter(catchment == "c1"),
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  expect_warning(
    compare_seroincidence(est_by, y = est1),
    class = "rlang_warning"
  )
})

test_that("compare_seroincidence produces mathematically correct results", {
  withr::local_package("dplyr")

  est1 <- est_seroincidence(
    pop_data = sees_pop_data_pk_100 |> filter(catchment == "c1"),
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  est2 <- est_seroincidence(
    pop_data = sees_pop_data_pk_100 |> filter(catchment == "c2"),
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  result <- compare_seroincidence(est1, est2)

  # Get summaries
  sum1 <- summary(est1, coverage = 0.95, verbose = FALSE)
  sum2 <- summary(est2, coverage = 0.95, verbose = FALSE)

  # Check that estimates match
  expect_equal(result$estimate["incidence rate 1"], sum1$incidence.rate)
  expect_equal(result$estimate["incidence rate 2"], sum2$incidence.rate)

  # Check that difference is computed correctly
  expected_diff <- sum1$incidence.rate - sum2$incidence.rate
  expect_equal(result$estimate["difference"], expected_diff)

  # Check that SE is computed correctly
  expected_se <- sqrt(sum1$SE^2 + sum2$SE^2)
  expected_z <- expected_diff / expected_se
  expect_equal(result$statistic["z"], expected_z, tolerance = 1e-10)

  # Check that p-value is computed correctly
  expected_p <- 2 * pnorm(-abs(expected_z))
  expect_equal(result$p.value, expected_p, tolerance = 1e-10)
})
