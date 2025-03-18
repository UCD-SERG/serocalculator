test_that(
  desc = "`load_sr_params()` produces expected results",
  code = {
    expect_no_error(
      sr_params_true <-
        load_sr_params(
          serocalculator_example("example_curve_params.rds")
        )
    )
  }
)

test_that(
  desc = "`load_sr_params()` produces error with non-curve data",
  code = {
    expect_error(
      sr_params_true <-
        load_sr_params(
          serocalculator_example("example_pop_data.rds")
        )
    )
  }
)

test_that(
  desc = "non filepath produces error",
  code = {
    expect_error(
      expect_warning(
        load_sr_params("non file path.csv")
      )
    )
  }
)
