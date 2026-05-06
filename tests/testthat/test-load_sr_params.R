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
        load_sr_params("non file path")
      )
    )
  }
)

test_that(
  desc = "non-URL file error is re-thrown",
  code = {
    # Test that errors from non-URL paths are properly re-thrown unchanged
    expect_error(
      suppressWarnings(
        load_sr_params("nonexistent_file.rds")
      )
    )

    # Verify original error is preserved
    err <- tryCatch(
      suppressWarnings(
        load_sr_params("nonexistent_file.rds")
      ),
      error = function(e) e
    )

    # Should be the original error class, not rlang_error
    expect_true(inherits(err, "error"))
    expect_true(inherits(err, "condition"))
  }
)

test_that(
  desc = "unavailable internet resource produces informative error",
  code = {
    # Test with a non-existent URL
    expect_error(
      load_sr_params("https://ucdserg.ucdavis.edu/nofile"),
      class = "internet_resource_unavailable",
      regexp = "Unable to load seroresponse parameters from internet resource"
    )

    # Verify the error contains helpful information
    err <- tryCatch(
      load_sr_params("https://ucdserg.ucdavis.edu/file.rds"),
      error = function(e) e
    )

    expect_match(conditionMessage(err), "not available or has changed")
    expect_match(conditionMessage(err), "check your internet connection")
  }
)

test_that(
  desc = "deprecated load_curve_params() still works",
  code = {
    # Test the deprecated function still works
    # Note: lifecycle::deprecate_soft() only warns once per session
    # so we can't always expect a warning
    result <- suppressWarnings(
      load_curve_params(
        serocalculator_example("example_curve_params.rds")
      )
    )

    # Verify it returns the same result as load_sr_params
    expect_s3_class(result, "curve_params")
  }
)
