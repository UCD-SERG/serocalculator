
test_that("`ids()` works", {
  xs_data <- load_pop_data(serocalculator_example("example_pop_data.rds"))

  xs_data |>
    ids() |>
    sort() |>
    expect_snapshot_value(style = "deparse")

})
