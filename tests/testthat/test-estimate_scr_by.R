test_that(
  desc = "estimate_scr_by() warns about missing data",
  code = {

    library(dplyr)
    library(readr)

    estimate_scr_by(
      pop_data =
        sees_pop_data_pk_100 |>
        dplyr::filter(catchment == "kgh" | antigen_iso == "HlyE_IgA"),
      curve_params = typhoid_curves_nostrat_100,
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


test_that("estimate_scr_by() warns about missing data", {

  library(dplyr)
  library(readr)

  estimate_scr_by(
    pop_data = sees_pop_data_pk_100 |>
      tail(-1),
    curve_params = typhoid_curves_nostrat_100,
    noise_params = example_noise_params_sees |>
      dplyr::filter(Country == "Nepal"),
    strata = "catchment",
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL
  ) |>
    expect_warning(class = "incomplete-obs")
})

test_that("`estimate_scr_by()` warns user when strata is missing", {
  expect_warning(
    estimate_scr_by(
      pop_data = sees_pop_data_pk_100,
      curve_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA")
    ),
    class = "strata_empty"
  )
})

test_that(
  "`estimate_scr_by()` aborts when elements that don't exactly
          match the columns of `pop_data` are provided",
  {
    expect_error(
      object = estimate_scr_by(
        strata = c("ag", "catch", "Count"),
        pop_data = sees_pop_data_pk_100,
        curve_params = typhoid_curves_nostrat_100,
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
  desc = "`estimate_scr_by()` produces consistent results for typhoid data",
  code = {
    withr::local_options(width = 80)
    typhoid_results <- estimate_scr_by(
      strata = "catchment",
      pop_data = sees_pop_data_pk_100,
      curve_param = typhoid_curves_nostrat_100,
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
  "`estimate_scr_by()` produces expected results
          regardless of whether varnames have been standardized.",
  {
    est_true <- estimate_scr_by(
      strata = c("catchment"),
      pop_data = sees_pop_data_pk_100,
      curve_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      num_cores = 1 # Allow for parallel processing to decrease run time
    )

    est_false <- estimate_scr_by(
      strata = c("catchment"),
      pop_data = sees_pop_data_pk_100_old_names,
      curve_params = typhoid_curves_nostrat_100,
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
  "`estimate_scr_by()` produces expected results
          regardless of whether using parallel processing or not.",
  {

    ests_1_core <- estimate_scr_by(
      strata = c("catchment"),
      pop_data = sees_pop_data_pk_100,
      curve_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      num_cores = 1
    )

    ests_2_cores <- estimate_scr_by(
      strata = c("catchment"),
      pop_data = sees_pop_data_pk_100_old_names,
      curve_params = typhoid_curves_nostrat_100,
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
  "`estimate_scr_by()` produces expected results
          regardless of whether using verbose messaging or not.
          with single core.",
  {

    capture.output(
      file = nullfile(),
      {
        ests_verbose_sc <- estimate_scr_by(
          strata = c("catchment"),
          pop_data = sees_pop_data_pk_100,
          curve_params = typhoid_curves_nostrat_100,
          noise_params = example_noise_params_pk,
          antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
          curve_strata_varnames = NULL,
          noise_strata_varnames = NULL,
          verbose = TRUE,
          num_cores = 1
        ) |> suppressMessages()
      }
    )

    ests_non_verbose_sc <- estimate_scr_by(
      verbose = FALSE,
      strata = c("catchment"),
      pop_data = sees_pop_data_pk_100_old_names,
      curve_params = typhoid_curves_nostrat_100,
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
  "`estimate_scr_by()` produces expected results
          regardless of whether using verbose messaging or not
          with multi-core processing.",
  {

    ests_verbose_mc <- estimate_scr_by(
      strata = c("catchment"),
      pop_data = sees_pop_data_pk_100,
      curve_params = typhoid_curves_nostrat_100,
      noise_params = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      verbose = TRUE,
      num_cores = 2
    ) |>
      suppressMessages()

    ests_non_verbose_mc <- estimate_scr_by(
      verbose = FALSE,
      strata = c("catchment"),
      pop_data = sees_pop_data_pk_100_old_names,
      curve_params = typhoid_curves_nostrat_100,
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
    estimate_scr_by(
      strata = NULL,
      pop_data = sees_pop_data_pk_100,
      curve_param = typhoid_curves_nostrat_100,
      noise_param = example_noise_params_pk,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA")
    ) |>
      expect_snapshot()
  }
)

test_that("results are consistent with `strata = NULL`", {
  typhoid_results_simple <- estimate_scr(
    pop_data = sees_pop_data_pk_100,
    curve_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )

  typhoid_results_nullstrata <- estimate_scr_by(
    strata = NULL,
    pop_data = sees_pop_data_pk_100,
    curve_param = typhoid_curves_nostrat_100,
    noise_param = example_noise_params_pk,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  ) |> suppressWarnings()

  expect_equal(
    typhoid_results_simple,
    typhoid_results_nullstrata
  )

})
