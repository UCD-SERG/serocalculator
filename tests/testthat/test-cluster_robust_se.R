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

test_that("CR1 increases variance when clusters are few", {
  withr::local_seed(20241213)

  test_data <- sees_pop_data_pk_100
  test_data$cluster_small <- rep(seq_len(4), length.out = nrow(test_data))

  est_cluster <- est_seroincidence(
    pop_data = test_data,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = "cluster_small"
  )
  pop_data_combined <- do.call(rbind, attr(est_cluster, "pop_data"))
  cluster_ids <- pop_data_combined$cluster_small

  var_no_correction <- .compute_cluster_var_oneway(
    fit = est_cluster,
    cluster_ids = cluster_ids,
    pop_data_combined = pop_data_combined,
    small_sample = "none"
  )
  var_cr1 <- .compute_cluster_var_oneway(
    fit = est_cluster,
    cluster_ids = cluster_ids,
    pop_data_combined = pop_data_combined,
    small_sample = "CR1"
  )

  expect_gt(var_cr1, var_no_correction)
  expect_equal(var_cr1, var_no_correction * (4 / 3), tolerance = 1e-10)
})

test_that("cluster decomposition stores signed multi-way terms", {
  withr::local_seed(20241213)

  test_data <- sees_pop_data_pk_100
  test_data$school <- rep(1:5, length.out = nrow(test_data))
  test_data$classroom <- rep(1:10, length.out = nrow(test_data))

  est_multi <- est_seroincidence(
    pop_data = test_data,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = c("school", "classroom")
  )
  robust_var <- .compute_cluster_robust_var(
    fit = est_multi,
    cluster_var = c("school", "classroom"),
    small_sample = "CR1",
    floor_to_standard = FALSE
  )
  decomp <- attr(robust_var, "cluster_decomp")

  expect_type(decomp, "list")
  expect_named(
    decomp,
    c("standard_var", "robust_raw", "robust_final", "terms", "floor_applied")
  )
  expect_equal(nrow(decomp$terms), 3)
  expect_equal(
    decomp$robust_raw,
    sum(decomp$terms$signed_term),
    tolerance = 1e-10
  )
  expect_equal(decomp$robust_final, as.numeric(robust_var), tolerance = 1e-10)
})

test_that("optional variance floor can be enabled explicitly", {
  withr::local_seed(20241213)

  test_data <- sees_pop_data_pk_100
  test_data$household <- seq_len(nrow(test_data))

  est_household <- est_seroincidence(
    pop_data = test_data,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = "household"
  )
  raw_var <- .compute_cluster_robust_var(
    fit = est_household,
    cluster_var = "household",
    small_sample = "none",
    floor_to_standard = FALSE
  )
  floored_var <- .compute_cluster_robust_var(
    fit = est_household,
    cluster_var = "household",
    small_sample = "none",
    floor_to_standard = TRUE
  )
  raw_decomp <- attr(raw_var, "cluster_decomp")
  floored_decomp <- attr(floored_var, "cluster_decomp")

  expect_equal(as.numeric(raw_var), raw_decomp$robust_raw, tolerance = 1e-10)
  expect_lt(raw_var, raw_decomp$standard_var)
  expect_false(raw_decomp$floor_applied)

  expect_equal(
    floored_decomp$robust_raw,
    as.numeric(raw_var),
    tolerance = 1e-10
  )
  expect_true(floored_decomp$floor_applied)
  expect_gte(floored_var, floored_decomp$standard_var)
})

test_that("nested multi-way clustering stays consistent within tolerance", {
  withr::local_seed(20241213)

  test_data <- sees_pop_data_pk_100
  test_data$household <- seq_len(nrow(test_data))
  test_data$commune <- rep(1:10, length.out = nrow(test_data))

  est_commune <- est_seroincidence(
    pop_data = test_data,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = "commune"
  )
  est_nested <- est_seroincidence(
    pop_data = test_data,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = c("commune", "household")
  )

  commune_var <- .compute_cluster_robust_var(
    fit = est_commune,
    cluster_var = "commune",
    small_sample = "CR1",
    floor_to_standard = FALSE
  )
  nested_var <- .compute_cluster_robust_var(
    fit = est_nested,
    cluster_var = c("commune", "household"),
    small_sample = "CR1",
    floor_to_standard = FALSE
  )
  nested_decomp <- attr(nested_var, "cluster_decomp")
  household_term <- nested_decomp$terms |>
    dplyr::filter(.data$subset == "household")
  intersection_term <- nested_decomp$terms |>
    dplyr::filter(.data$subset == "commune + household")

  expect_equal(
    household_term$subset_variance,
    intersection_term$subset_variance,
    tolerance = 1e-10
  )
  expect_equal(
    nested_decomp$robust_raw,
    as.numeric(commune_var),
    tolerance = 1e-10
  )
})

test_that("debug output reports two-way variance terms", {
  withr::local_seed(20241213)

  test_data <- sees_pop_data_pk_100
  test_data$household <- seq_len(nrow(test_data))
  test_data$commune <- rep(1:10, length.out = nrow(test_data))

  est_nested <- est_seroincidence(
    pop_data = test_data,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = c("commune", "household")
  )
  debug_output <- testthat::capture_messages(
    summary(
      est_nested,
      verbose = FALSE,
      debug_cluster = TRUE
    )
  )
  debug_output <- paste(debug_output, collapse = "\n")

  expect_match(debug_output, "V_commune")
  expect_match(debug_output, "V_household")
  expect_match(debug_output, "V_intersection")
  expect_match(debug_output, "V_raw")
  expect_match(debug_output, "V_final")
})

test_that("negative multi-way variance is floored at zero with a warning", {
  decomp_terms <- tibble::tibble(
    subset = c("a", "b", "a + b"),
    order = c(1, 1, 2),
    sign = c(1, 1, -1),
    subset_variance = c(1, 1, 5),
    signed_term = c(1, 1, -5)
  )

  expect_warning(
    result <- .combine_cluster_decomp(
      decomp_terms = decomp_terms,
      standard_var = 0.2,
      floor_to_standard = FALSE
    ),
    "negative"
  )

  # raw sum is preserved for inspection, but the returned variance is floored
  expect_equal(result$robust_raw, -3)
  expect_equal(result$robust_final, 0)
  expect_false(result$floor_applied)
})

test_that("positive multi-way variance passes through unfloored", {
  decomp_terms <- tibble::tibble(
    subset = c("a", "b", "a + b"),
    order = c(1, 1, 2),
    sign = c(1, 1, -1),
    subset_variance = c(3, 3, 1),
    signed_term = c(3, 3, -1)
  )

  result <- .combine_cluster_decomp(
    decomp_terms = decomp_terms,
    standard_var = 0.2,
    floor_to_standard = FALSE
  )

  expect_equal(result$robust_raw, 5)
  expect_equal(result$robust_final, 5)
  expect_false(result$floor_applied)
})

test_that("floor_to_standard raises a floored-at-zero estimate to standard", {
  decomp_terms <- tibble::tibble(
    subset = c("a", "b", "a + b"),
    order = c(1, 1, 2),
    sign = c(1, 1, -1),
    subset_variance = c(1, 1, 5),
    signed_term = c(1, 1, -5)
  )

  suppressWarnings(
    result <- .combine_cluster_decomp(
      decomp_terms = decomp_terms,
      standard_var = 0.2,
      floor_to_standard = TRUE
    )
  )

  expect_equal(result$robust_final, 0.2)
  expect_true(result$floor_applied)
})

test_that("degenerate Hessian warns and returns NA for a one-way term", {
  withr::local_seed(20241213)

  test_data <- sees_pop_data_pk_100
  test_data$cluster_small <- rep(seq_len(4), length.out = nrow(test_data))

  est_cluster <- est_seroincidence(
    pop_data = test_data,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = "cluster_small"
  )
  # force a degenerate Hessian to exercise the guard
  est_cluster$hessian <- -1
  pop_data_combined <- do.call(rbind, attr(est_cluster, "pop_data"))

  expect_warning(
    degenerate_var <- .compute_cluster_var_oneway(
      fit = est_cluster,
      cluster_ids = pop_data_combined$cluster_small,
      pop_data_combined = pop_data_combined,
      small_sample = "none"
    ),
    "Hessian"
  )
  expect_true(is.na(degenerate_var))
})
