test_that(
  desc = "`load_noise_params()` produces expected results",
  code = {
    expect_no_error(
      noise_params_true <-
        load_noise_params(
          serocalculator_example("example_noise_params.rds")
        )
    )
  }
)

test_that(
  desc = "non-URL file error is re-thrown",
  code = {
    # Test that errors from non-URL paths are properly re-thrown
    expect_error(
      suppressWarnings(
        load_noise_params("nonexistent_file.rds")
      ),
      class = "rlang_error"
    )
  }
)

test_that(
  desc = "unavailable internet resource produces informative error",
  code = {
    # Test with a non-existent URL
    expect_error(
      load_noise_params("http://nonexistent.example.com/file.rds"),
      class = "internet_resource_unavailable",
      regexp = "Unable to load noise parameters from internet resource"
    )

    # Verify the error contains helpful information
    err <- tryCatch(
      load_noise_params("http://nonexistent.example.com/file.rds"),
      error = function(e) e
    )

    expect_match(conditionMessage(err), "not available or has changed")
    expect_match(conditionMessage(err), "check your internet connection")
  }
)
