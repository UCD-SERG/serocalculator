
test_that("`ids()` works", {
  sees_pop_data_pk_100 |>
    ids() |>
    expect_snapshot_value(style = "deparse")

})


test_that("`id_var()` warns when guessing attributes", {
  xs_data <- serocalculator_example("example_pop_data.rds") |>
    readr::read_rds()
  xs_data |>
    ids_varname() |>
    expect_snapshot()

})
