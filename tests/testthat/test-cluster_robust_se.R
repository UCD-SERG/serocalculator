test_that("cluster-robust standard errors work correctly", {
  # Test with typhoid data that has cluster information
  withr::local_seed(20241213)

  # Run without clustering
  est_no_cluster <- est_seroincidence(
    pop_data = sees_pop_data_pk_100,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  # Run with clustering
  est_with_cluster <- est_seroincidence(
    pop_data = sees_pop_data_pk_100,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = "cluster"
  )

  # Both should have same point estimate
  expect_equal(
    est_no_cluster$estimate,
    est_with_cluster$estimate
  )

  # Get summaries
  sum_no_cluster <- summary(est_no_cluster, verbose = FALSE)
  sum_with_cluster <- summary(est_with_cluster, verbose = FALSE)

  # Point estimates should be the same
  expect_equal(
    sum_no_cluster$incidence.rate,
    sum_with_cluster$incidence.rate
  )

  # Standard errors should generally be different
  # (typically larger with clustering)
  # We can't test direction reliably, but should exist and be positive
  expect_true(sum_no_cluster$SE > 0)
  expect_true(sum_with_cluster$SE > 0)

  # Confidence intervals should be valid
  expect_true(sum_with_cluster$CI.lwr < sum_with_cluster$incidence.rate)
  expect_true(sum_with_cluster$CI.upr > sum_with_cluster$incidence.rate)

  # Check se_type column exists and has correct values
  expect_true("se_type" %in% names(sum_no_cluster))
  expect_true("se_type" %in% names(sum_with_cluster))
  expect_equal(sum_no_cluster$se_type, "standard")
  expect_equal(sum_with_cluster$se_type, "cluster-robust")
})

test_that("cluster_var validation works", {
  # Test with invalid cluster_var
  expect_error(
    est_seroincidence(
      pop_data = sees_pop_data_pk_100,
      sr_param = typhoid_curves_nostrat_100,
      noise_param = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      cluster_var = "nonexistent_var"
    ),
    "is not a column"
  )
})

test_that("stratum_var validation works", {
  # Test with invalid stratum_var
  expect_error(
    est_seroincidence(
      pop_data = sees_pop_data_pk_100,
      sr_param = typhoid_curves_nostrat_100,
      noise_param = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      stratum_var = "nonexistent_stratum"
    ),
    "is not a column"
  )
})

test_that("cluster and stratum variables together", {
  # Test with both cluster and stratum
  withr::local_seed(20241213)

  est_cluster_stratum <- est_seroincidence(
    pop_data = sees_pop_data_pk_100,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = "cluster",
    stratum_var = "catchment"
  )

  sum_result <- summary(est_cluster_stratum, verbose = FALSE)

  # Should produce valid results
  expect_true(sum_result$SE > 0)
  expect_true(sum_result$CI.lwr < sum_result$incidence.rate)
  expect_true(sum_result$CI.upr > sum_result$incidence.rate)
})

test_that("sampling_weights parameter shows warning", {
  # sampling_weights not yet implemented
  expect_warning(
    est_seroincidence(
      pop_data = sees_pop_data_pk_100,
      sr_param = typhoid_curves_nostrat_100,
      noise_param = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      sampling_weights = data.frame(cluster = "test", weight = 1)
    ),
    "not yet implemented"
  )
})

test_that("clustering by subject id gives a composite-likelihood SE (#645)", {
  # `log_likelihood()` combines biomarkers by summing their marginal
  # log-likelihoods (see #637), which is only valid if those
  # contributions are independent.
  # Two biomarker readings from the same person usually aren't, since
  # they share an infection history --- so clustering the
  # cluster-robust score on the subject id is the
  # composite-likelihood-appropriate correction (#645).
  # This locks in today's naive SE and confirms the two differ, in
  # the expected direction, on real multi-biomarker data.

  id_var <- ids_varname(sees_pop_data_pk_100)

  est_naive <- est_seroincidence(
    pop_data = sees_pop_data_pk_100,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  est_composite <- est_seroincidence(
    pop_data = sees_pop_data_pk_100,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = id_var
  )

  # clustering only affects inference, not the point estimate
  expect_equal(est_naive$estimate, est_composite$estimate)

  sum_naive <- summary(est_naive, verbose = FALSE)
  sum_composite <- summary(est_composite, verbose = FALSE)

  expect_equal(sum_naive$se_type, "standard")
  expect_equal(sum_composite$se_type, "cluster-robust")

  # locks in today's naive SE, reproduced exactly, so a future change
  # to the underlying model (e.g. #646) doesn't silently alter it
  expect_snapshot_value(sum_naive$SE, style = "deparse", tolerance = 1e-6)

  # A sandwich SE isn't guaranteed to exceed the naive one in general
  # (it depends on the sign of the within-cluster score correlation),
  # so this isn't a property of the estimator.
  # It's an empirical regression check against `sees_pop_data_pk_100`,
  # a fixed, bundled dataset.
  # It's the direction we expect for this fixture, since biomarkers
  # from the same person are positively correlated here (shared
  # infection history).
  # A deliberate change to the underlying model (e.g. #646) may need
  # to update this value.
  expect_gt(sum_composite$SE, sum_naive$SE)
})

test_that("multiple cluster variables work correctly", {
  withr::local_seed(20241213)
  
  # Create test data with multiple clustering levels
  test_data <- sees_pop_data_pk_100
  test_data$school <- rep(1:5, length.out = nrow(test_data))
  test_data$classroom <- rep(1:10, length.out = nrow(test_data))
  
  # Fit with multiple cluster variables
  est_multi <- est_seroincidence(
    pop_data = test_data,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = c("school", "classroom")
  )
  
  # Should succeed
  expect_s3_class(est_multi, "seroincidence")
  
  # Check that cluster_var attribute has both variables
  expect_equal(attr(est_multi, "cluster_var"), c("school", "classroom"))
  
  # Summary should work
  sum_multi <- summary(est_multi, verbose = FALSE)
  expect_equal(sum_multi$se_type, "cluster-robust")
  
  # Standard errors should be positive
  expect_true(sum_multi$SE > 0)
})

test_that("nested two-way clustering uses multi-way correction", {
  withr::local_seed(20241213)

  test_data <- sees_pop_data_pk_100 |>
    dplyr::mutate(
      commune = .data$cluster,
      household_id = paste0(
        .data$cluster,
        "_",
        (as.integer(factor(.data$id)) %% 5) + 1
      )
    )

  sum_standard <- est_seroincidence(
    pop_data = test_data,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  ) |>
    summary(verbose = FALSE)

  sum_commune <- est_seroincidence(
    pop_data = test_data,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = "commune"
  ) |>
    summary(verbose = FALSE)

  sum_household <- est_seroincidence(
    pop_data = test_data,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = "household_id"
  ) |>
    summary(verbose = FALSE)

  sum_two_way <- est_seroincidence(
    pop_data = test_data,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = c("commune", "household_id")
  ) |>
    summary(verbose = FALSE)

  expect_equal(sum_standard$se_type, "standard")
  expect_equal(sum_commune$se_type, "cluster-robust")
  expect_equal(sum_household$se_type, "cluster-robust")
  expect_equal(sum_two_way$se_type, "cluster-robust")

  # For nested clustering (households within communes), two-way correction
  # should reduce to the coarser commune clustering rather than household-only.
  expect_equal(sum_two_way$SE, sum_commune$SE, tolerance = 1e-6)
  expect_false(isTRUE(all.equal(sum_two_way$SE, sum_household$SE)))
})
