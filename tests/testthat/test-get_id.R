
test_that("`get_person_ids()` works", {
  xs_data <- load_pop_data(serocalculator_example("example_pop_data.rds"))

  xs_data |>
    get_person_ids() |>
    sort() |>
    expect_snapshot_value(style = "deparse")

})
