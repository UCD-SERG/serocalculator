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
  desc = "unavailable internet resource produces informative error",
  code = {
    # Test with a non-existent URL
    expect_error(
      load_sr_params("http://nonexistent.example.com/file.rds"),
      class = "internet_resource_unavailable",
      regexp = "Unable to load seroresponse parameters from internet resource"
    )
    
    # Verify the error contains helpful information
    err <- tryCatch(
      load_sr_params("http://nonexistent.example.com/file.rds"),
      error = function(e) e
    )
    
    expect_match(conditionMessage(err), "not available or has changed")
    expect_match(conditionMessage(err), "check your internet connection")
  }
)
