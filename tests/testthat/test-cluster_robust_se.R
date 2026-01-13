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
