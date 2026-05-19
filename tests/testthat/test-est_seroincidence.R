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

test_that("clustering works with est_seroincidence", {
  # Test with cluster_var
  est_cluster <- est_seroincidence(
    pop_data = sees_pop_data_pk_100,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = "cluster"
  )
  
  # Should have cluster_var attribute
  expect_equal(attr(est_cluster, "cluster_var"), "cluster")
  
  # Summary should work
  sum_cluster <- summary(est_cluster, verbose = FALSE)
  expect_true("se_type" %in% names(sum_cluster))
  expect_equal(sum_cluster$se_type, "cluster-robust")
  
  # Should not have [] in column names
  expect_false(any(grepl("\\[", names(sum_cluster))))
})

test_that("clustering with stratum works with est_seroincidence", {
  # Test with both cluster_var and stratum_var
  est_both <- est_seroincidence(
    pop_data = sees_pop_data_pk_100,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    cluster_var = "cluster",
    stratum_var = "catchment"
  )
  
  # Should have both attributes
  expect_equal(attr(est_both, "cluster_var"), "cluster")
  expect_equal(attr(est_both, "stratum_var"), "catchment")
  
  # Summary should work
  sum_both <- summary(est_both, verbose = FALSE)
  expect_equal(sum_both$se_type, "cluster-robust")
  
  # Verify functional impact: clustering should affect standard errors
  # Compare with non-clustered version
  est_no_cluster <- est_seroincidence(
    pop_data = sees_pop_data_pk_100,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )
  
  sum_no_cluster <- summary(est_no_cluster, verbose = FALSE)
  
  # Point estimates should be identical
  expect_equal(sum_both$incidence.rate, sum_no_cluster$incidence.rate)
  
  # Cluster-robust SE should generally be larger due to correlation
  # (though in rare cases with negative ICC, it could be smaller)
  # At minimum, they should differ when there's actual clustering
  expect_false(isTRUE(all.equal(sum_both$SE, sum_no_cluster$SE)))
})

test_that("invalid cluster_var causes error", {
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

test_that("invalid stratum_var causes error", {
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
