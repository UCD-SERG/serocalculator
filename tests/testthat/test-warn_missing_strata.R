test_that("`test_missing_strata()` works", {
  expected_strata = data.frame(Species = "banana", type = "orchid")

  warn_missing_strata(iris, expected_strata, dataname = "iris") |>
    capture.output() |> # not sure this works as intended
    suppressMessages() |>
    expect_error() |>
    expect_warning()

})
