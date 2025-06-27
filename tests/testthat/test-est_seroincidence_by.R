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
  desc = "`est_seroincidence_by()` produces consistent results for typhoid data",
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

    ests_1_core <- est_seroincidence_by(
      strata = c("catchment"),
      pop_data = sees_pop_data_pk_100,
      sr_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      num_cores = 1
    )

    ests_2_cores <- est_seroincidence_by(
      strata = c("catchment"),
      pop_data = sees_pop_data_pk_100_old_names,
      sr_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      num_cores = 2
    )

    expect_equal(ests_1_core, ests_2_cores)
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
    curve_strata_varnames= NULL,
    noise_strata_varnames = NULL,
    noise_params = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    build_graph = TRUE
  ) |>
    expect_warning()
})
