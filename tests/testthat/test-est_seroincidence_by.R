test_that(
  desc = "est_seroincidence_by() warns about missing data",
  code = {

    withr::local_package("dplyr")
    withr::local_package("readr")

    est_seroincidence_by(
      pop_data =
        sees_pop_data_pk_100 |>
        dplyr::filter(catchment == "kgh" | antigen_iso == "HlyE_IgA"),
      sr_params = typhoid_curves_nostrat_100,
      noise_params =
        example_noise_params_sees |>
        dplyr::filter(Country == "Nepal"),
      strata = "catchment",
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL
    ) |>
      expect_warning(class = "strata missing some biomarkers")
  }
)


test_that("est_seroincidence_by() warns about missing data", {

  withr::local_package("dplyr")
  withr::local_package("readr")


  est_seroincidence_by(
    pop_data = sees_pop_data_pk_100 |>
      tail(-1),
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_sees |>
      dplyr::filter(Country == "Nepal"),
    strata = "catchment",
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL
  ) |>
    expect_warning(class = "incomplete-obs")
})

test_that("`est_seroincidence_by()` warns user when strata is missing", {
  expect_warning(
    est_seroincidence_by(
      pop_data = sees_pop_data_pk_100,
      sr_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA")
    ),
    class = "strata_empty"
  )
})

test_that(
  "`est_seroincidence_by()` aborts when elements that don't exactly
          match the columns of `pop_data` are provided",
  {
    expect_error(
      object = est_seroincidence_by(
        strata = c("ag", "catch", "Count"),
        pop_data = sees_pop_data_pk_100,
        sr_params = typhoid_curves_nostrat_100,
        noise_params = example_noise_params_pk,
        antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
        num_cores = 8,
        # Allow for parallel processing to decrease run time
        iterlim = 5 # limit iterations for the purpose of this example
      ),
      class = "missing_var"
    )

  }
)


test_that(
  desc = "`est_seroincidence_by()` produces consistent results for sample data",
  code = {
    withr::local_options(width = 80)
    typhoid_results <- est_seroincidence_by(
      strata = "catchment",
      pop_data = sees_pop_data_pk_100,
      sr_param = typhoid_curves_nostrat_100,
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      noise_param = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      # Allow for parallel processing to decrease run time
      num_cores = 1
    )

    expect_snapshot_value(typhoid_results,
                          style = "deparse",
                          tolerance = 1e-4)

  }
)

test_that(
  "`est_seroincidence_by()` produces expected results
          regardless of whether varnames have been standardized.",
  {
    est_true <- est_seroincidence_by(
      strata = c("catchment"),
      pop_data = sees_pop_data_pk_100,
      sr_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      num_cores = 1 # Allow for parallel processing to decrease run time
    )

    est_false <- est_seroincidence_by(
      strata = c("catchment"),
      pop_data = sees_pop_data_pk_100_old_names,
      sr_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      num_cores = 1 # Allow for parallel processing to decrease run time
    )

    expect_equal(est_true, est_false)
  }
)


test_that(
  "`est_seroincidence_by()` produces expected results
          regardless of whether using parallel processing or not.",
  {

    ests_single_core <- est_seroincidence_by(
      strata = c("catchment"),
      pop_data = sees_pop_data_pk_100,
      sr_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      num_cores = 1
    )

    ests_multi_cores <- est_seroincidence_by(
      strata = c("catchment"),
      pop_data = sees_pop_data_pk_100_old_names,
      sr_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      num_cores = 2
    )

    expect_equal(ests_single_core, ests_multi_cores)
  }
)

test_that(
  "`est_seroincidence_by()` produces expected results
          regardless of whether using verbose messaging or not.
          with single core.",
  {

    capture.output(
      file = nullfile(),
      {
        ests_verbose_sc <- est_seroincidence_by(
          strata = c("catchment"),
          pop_data = sees_pop_data_pk_100,
          sr_params = typhoid_curves_nostrat_100,
          noise_params = example_noise_params_pk,
          antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
          curve_strata_varnames = NULL,
          noise_strata_varnames = NULL,
          verbose = TRUE,
          num_cores = 1
        ) |> suppressMessages()
      }
    )

    ests_non_verbose_sc <- est_seroincidence_by(
      verbose = FALSE,
      strata = c("catchment"),
      pop_data = sees_pop_data_pk_100_old_names,
      sr_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      num_cores = 1
    )

    expect_equal(ests_verbose_sc, ests_non_verbose_sc)
  }
)

test_that(
  "`est_seroincidence_by()` produces expected results
          regardless of whether using verbose messaging or not
          with multi-core processing.",
  {

    ests_verbose_mc <- est_seroincidence_by(
      strata = c("catchment"),
      pop_data = sees_pop_data_pk_100,
      sr_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      verbose = TRUE,
      num_cores = 2
    ) |>
      suppressMessages()

    ests_non_verbose_mc <- est_seroincidence_by(
      verbose = FALSE,
      strata = c("catchment"),
      pop_data = sees_pop_data_pk_100_old_names,
      sr_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      num_cores = 2
    )

    expect_equal(ests_verbose_mc, ests_non_verbose_mc)
  }
)

# note: no need to check multi-core verbose vs single-core, nonverbose,
# or the other diagonal, because of transitive equality and the three checks
# made above

test_that(
  "a warning is produced when `strata = NULL",
  code = {
    est_seroincidence_by(
      strata = NULL,
      pop_data = sees_pop_data_pk_100,
      sr_param = typhoid_curves_nostrat_100,
      noise_param = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA")
    ) |>
      expect_snapshot()
  }
)

test_that("results are consistent with `strata = NULL`", {
  typhoid_results_simple <- est_seroincidence(
    pop_data = sees_pop_data_pk_100,
    sr_params = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  typhoid_results_nullstrata <- est_seroincidence_by(
    strata = NULL,
    pop_data = sees_pop_data_pk_100,
    sr_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  ) |> suppressWarnings()

  expect_equal(
    typhoid_results_simple,
    typhoid_results_nullstrata
  )

})

test_that("deprecate warning is as expected", {
  est2 <- est.incidence.by(
    strata = c("catchment"),
    pop_data = sees_pop_data_pk_100,
    sr_params = typhoid_curves_nostrat_100,
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    build_graph = TRUE
  ) |>
    expect_warning()
})

test_that("clustering works with est_seroincidence_by", {
  # Test with cluster_var
  est_cluster <- est_seroincidence_by(
    strata = "catchment",
    pop_data = sees_pop_data_pk_100,
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL,
    cluster_var = "cluster",
    num_cores = 1
  )
  
  # Should be seroincidence.by object
  expect_s3_class(est_cluster, "seroincidence.by")
  
  # Each stratum should have cluster_var attribute
  for (stratum_name in names(est_cluster)) {
    expect_equal(attr(est_cluster[[stratum_name]], "cluster_var"), "cluster")
  }
  
  # Summary should work and have se_type
  sum_cluster <- summary(est_cluster)
  expect_true("se_type" %in% names(sum_cluster))
  # sum_cluster has one row per stratum, check all are cluster-robust
  expect_true(all(sum_cluster$se_type == "cluster-robust"))
})

test_that("clustering with stratum works with est_seroincidence_by", {
  # Test with both cluster_var and stratum_var
  est_both <- est_seroincidence_by(
    strata = "catchment",
    pop_data = sees_pop_data_pk_100,
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL,
    cluster_var = "cluster",
    stratum_var = "catchment",
    num_cores = 1
  )
  
  # Each stratum should have both attributes
  for (stratum_name in names(est_both)) {
    expect_equal(attr(est_both[[stratum_name]], "cluster_var"), "cluster")
    expect_equal(attr(est_both[[stratum_name]], "stratum_var"), "catchment")
  }
  
  # Verify functional impact: clustering should affect standard errors
  # Compare with non-clustered version
  est_no_cluster <- est_seroincidence_by(
    strata = "catchment",
    pop_data = sees_pop_data_pk_100,
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL,
    num_cores = 1
  )
  
  sum_both <- summary(est_both)
  sum_no_cluster <- summary(est_no_cluster)
  
  # Point estimates should be identical across all strata
  expect_equal(sum_both$incidence.rate, sum_no_cluster$incidence.rate)
  
  # Standard errors should differ when clustering is applied
  # (may be larger or smaller depending on within-cluster correlation)
  expect_false(isTRUE(all.equal(sum_both$SE, sum_no_cluster$SE)))
  
  # Verify se_type is correctly set
  expect_true(all(sum_both$se_type == "cluster-robust"))
  expect_true(all(sum_no_cluster$se_type == "standard"))
})

test_that("clustering works with parallel processing", {
  skip_on_cran()
  skip("Parallel processing with clustering needs investigation")
  
  # Test with cluster_var and parallel processing
  est_cluster_parallel <- est_seroincidence_by(
    strata = "catchment",
    pop_data = sees_pop_data_pk_100,
    sr_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL,
    cluster_var = "cluster",
    num_cores = 2
  )
  
  # Should work without errors
  expect_s3_class(est_cluster_parallel, "seroincidence.by")
  
  # Each stratum should have cluster_var attribute
  for (stratum_name in names(est_cluster_parallel)) {
    expect_equal(
      attr(est_cluster_parallel[[stratum_name]], "cluster_var"),
      "cluster"
    )
  }
})
