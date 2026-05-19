
test_that("`ids()` works", {
  sees_pop_data_pk_100 |>
    ids() |>
    expect_snapshot_value(style = "deparse")

})

test_that("`id_varname()` aborts with `.data` pronoun", {
  sees_pop_data_pk_100 |>
    dplyr::filter(
      ids(.data) == "P1"
    ) |>
    expect_snapshot(error = TRUE)

})

test_that("`ids_varname()` warns when guessing colname", {
  xs_data <- serocalculator_example("example_pop_data.rds") |>
    readr::read_rds()
  xs_data |>
    ids_varname() |>
    expect_snapshot()

})


test_that("`ids_varname()` warns when unable to guess colname", {
  xs_data <- serocalculator_example("example_pop_data.rds") |>
    readr::read_rds()
  xs_data |>
    dplyr::select(-index_id) |>
    ids_varname() |>
    expect_snapshot(error = TRUE)

})
