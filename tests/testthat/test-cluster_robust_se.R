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

test_that("compute_icc works correctly", {
  # Test with typhoid data that has cluster information
  withr::local_seed(20241213)

  # Run with clustering
  est_with_cluster <- est_seroincidence(
    pop_data = sees_pop_data_pk_100,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = "cluster"
  )

  # Compute ICC
  icc_result <- compute_icc(est_with_cluster)

  # Check structure
  expect_s3_class(icc_result, "icc_seroincidence")
  expect_true(is.list(icc_result))
  expect_true("icc" %in% names(icc_result))
  expect_true("deff" %in% names(icc_result))
  expect_true("avg_cluster_size" %in% names(icc_result))
  expect_true("n_clusters" %in% names(icc_result))
  expect_true("cluster_var" %in% names(icc_result))
  expect_true("antigen_isos" %in% names(icc_result))

  # Check values are reasonable
  expect_true(is.numeric(icc_result$icc))
  expect_true(is.numeric(icc_result$deff))
  expect_true(icc_result$deff > 0)
  expect_true(icc_result$avg_cluster_size > 0)
  expect_true(icc_result$n_clusters > 0)
  expect_equal(icc_result$cluster_var, "cluster")
  expect_true(grepl("HlyE_IgG", icc_result$antigen_isos))

  # DEFF should be >= 0 (typically >= 1 for positive ICC)
  expect_true(icc_result$deff >= 0)

  # Print should work without error
  expect_no_error(print(icc_result))
})

test_that("compute_icc works with est_seroincidence_by", {
  # Test with stratified data
  withr::local_seed(20241213)

  # Run with clustering and stratification
  est_by_cluster <- est_seroincidence_by(
    pop_data = sees_pop_data_pk_100,
    strata = "catchment",
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = "cluster",
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL
  ) |> suppressWarnings()

  # Compute ICC
  icc_result <- compute_icc(est_by_cluster)

  # Check structure
  expect_s3_class(icc_result, "icc_seroincidence.by")
  expect_true(is.data.frame(icc_result))
  
  # Check required columns
  expect_true("Stratum" %in% names(icc_result))
  expect_true("icc" %in% names(icc_result))
  expect_true("deff" %in% names(icc_result))
  expect_true("avg_cluster_size" %in% names(icc_result))
  expect_true("n_clusters" %in% names(icc_result))
  expect_true("cluster_var" %in% names(icc_result))
  expect_true("antigen_isos" %in% names(icc_result))
  expect_true("catchment" %in% names(icc_result))

  # Should have 2 rows (one per stratum)
  expect_equal(nrow(icc_result), 2)

  # All cluster_var should be "cluster"
  expect_true(all(icc_result$cluster_var == "cluster"))
  
  # All should have antigen_isos specified
  expect_true(all(grepl("HlyE_IgG", icc_result$antigen_isos)))

  # Print should work without error
  expect_no_error(print(icc_result))
})

test_that("compute_icc fails without clustering", {
  # Test with typhoid data without clustering
  withr::local_seed(20241213)

  est_no_cluster <- est_seroincidence(
    pop_data = sees_pop_data_pk_100,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  # Should fail when no clustering was used
  expect_error(
    compute_icc(est_no_cluster),
    "cluster_var"
  )
})

test_that("compute_icc fails with invalid input", {
  # Should fail with non-seroincidence object
  expect_error(
    compute_icc(list(a = 1)),
    "seroincidence"
  )
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

test_that("compute_icc returns min and max cluster sizes", {
  withr::local_seed(20241213)
  
  # Run with clustering
  est_with_cluster <- est_seroincidence(
    pop_data = sees_pop_data_pk_100,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = "cluster"
  )
  
  # Compute ICC
  icc_result <- compute_icc(est_with_cluster)
  
  # Check that min and max cluster sizes are present
  expect_true("min_cluster_size" %in% names(icc_result))
  expect_true("max_cluster_size" %in% names(icc_result))
  
  # Check values are reasonable
  expect_true(is.numeric(icc_result$min_cluster_size))
  expect_true(is.numeric(icc_result$max_cluster_size))
  expect_true(icc_result$min_cluster_size >= 0)
  expect_true(icc_result$max_cluster_size >= icc_result$min_cluster_size)
  expect_true(icc_result$avg_cluster_size >= icc_result$min_cluster_size)
  expect_true(icc_result$avg_cluster_size <= icc_result$max_cluster_size)
  
  # Print should work without error
  expect_no_error(print(icc_result))
})

test_that("compute_icc errors with multiple cluster variables", {
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
  
  # ICC should error with helpful message
  expect_error(
    compute_icc(est_multi),
    "ICC calculation only allowed for one level of clustering"
  )
  expect_error(
    compute_icc(est_multi),
    "school and classroom"
  )
})

test_that("compute_icc works with est_seroincidence_by and shows min/max", {
  withr::local_seed(20241213)
  
  # Run with clustering and stratification
  est_by_cluster <- est_seroincidence_by(
    pop_data = sees_pop_data_pk_100,
    strata = "catchment",
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = "cluster",
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL
  ) |> suppressWarnings()
  
  # Compute ICC
  icc_result <- compute_icc(est_by_cluster)
  
  # Check structure
  expect_s3_class(icc_result, "icc_seroincidence.by")
  expect_true(is.data.frame(icc_result))
  
  # Check that min and max cluster sizes are present
  expect_true("min_cluster_size" %in% names(icc_result))
  expect_true("max_cluster_size" %in% names(icc_result))
  
  # All values should be numeric and reasonable
  expect_true(all(icc_result$min_cluster_size >= 0))
  expect_true(all(icc_result$max_cluster_size >= icc_result$min_cluster_size))
  expect_true(
    all(icc_result$avg_cluster_size >= icc_result$min_cluster_size)
  )
  expect_true(
    all(icc_result$avg_cluster_size <= icc_result$max_cluster_size)
  )
})
