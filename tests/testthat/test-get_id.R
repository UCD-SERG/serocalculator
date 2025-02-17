
test_that("`get_id()` works", {
  xs_data <- load_pop_data(serocalculator_example("example_pop_data.rds"))

  xs_data |>
    get_id() |>
    sort() |>
    expect_snapshot_value(style = "deparse")

})
