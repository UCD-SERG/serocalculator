test_that("`get_biomarker_levels()` works", {
  xs_data <- load_pop_data(serocalculator_example("example_pop_data.rds"))
  biomarker_levels <- xs_data |> get_biomarker_levels()
  expected_levels <- c("HlyE_IgA", "HlyE_IgG")

  expect_equal(object = biomarker_levels, expected = expected_levels)

})

test_that("`set_age()` detects partial matches", {
  serocalculator_example("example_pop_data.rds") |>
    load_pop_data(age = "age$") |>
    expect_warning(class = "missing variable")
})
