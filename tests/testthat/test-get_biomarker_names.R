test_that("function works as expected", {
  biomarker_names <-
    serocalculator_example("example_pop_data.rds") |>
    load_pop_data() |>
    get_biomarker_names()

  expect_snapshot_value(biomarker_names, style = "deparse")
})
