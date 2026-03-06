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
      load_sr_params("non file path")
    )
  }
)

test_that(
  desc = "non-URL file error is re-thrown",
  code = {
    err <- tryCatch(
      load_sr_params("nonexistent_file.rds"),
      error = function(e) e
    )

    expect_true(inherits(err, "error"))
  }
)

test_that(
  desc = "unavailable internet resource produces informative error",
  code = {
    expect_error(
      load_sr_params("https://ucdserg.ucdavis.edu/nofile.rds"),
      class = "internet_resource_unavailable",
      regexp = "Unable to load seroresponse parameters from internet resource"
    )
  }
)

test_that(
  desc = "deprecated load_curve_params() still works",
  code = {
    result <- suppressWarnings(
      load_curve_params(
        serocalculator_example("example_curve_params.rds")
      )
    )

    expect_s3_class(result, "curve_params")
  }
)
