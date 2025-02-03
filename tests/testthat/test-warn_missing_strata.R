test_that("`test_missing_strata()` errors on absent strata", {
  expected_strata <- data.frame(Species = "banana")

  warn_missing_strata(iris, expected_strata, dataname = "iris") |>
    expect_error(class = "absent strata levels")
})

test_that("`test_missing_strata()` warns on missing strata vars", {
  expected_strata <- data.frame(Species = "setosa", type = "orchid")

  warn_missing_strata(iris, expected_strata, dataname = "iris") |>
    expect_warning()
})
