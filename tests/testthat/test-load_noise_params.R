test_that(
  desc = "`load_noise_params()` produces expected results",
  code = {
    expect_no_error(
      noise_params_true <-
        load_noise_params(
          serocalculator_example("example_noise_params.rds")
        )
    )
    expect_s3_class(noise_params_true, "noise_params")
  }
)

test_that(
  desc = "non-URL file error is re-thrown",
  code = {
    err <- tryCatch(
      load_noise_params("nonexistent_file.rds"),
      error = function(e) e
    )

    expect_true(inherits(err, "error"))
  }
)

test_that(
  desc = "unavailable internet resource produces informative error",
  code = {
    expect_error(
      load_noise_params("https://ucdserg.ucdavis.edu/nofile.rds"),
      class = "internet_resource_unavailable",
      regexp = "Unable to load noise parameters from internet resource"
    )
  }
)
