test_that("warns when specified id not found", {
  expect_snapshot(
    cnd_class = TRUE,
    x = {
      xs_data <- serocalculator_example("example_pop_data.rds") |>
        readr::read_rds() |>
        set_id_var(id = "id")
    }
  )
})

test_that(
  desc =
    "aborts when specified id not found and no partial match found",
  code = {
    expect_snapshot(
      error = TRUE,
      cnd_class = TRUE,
      x = {
        xs_data <- serocalculator_example("example_pop_data.rds") |>
          readr::read_rds() |>
          dplyr::select(-index_id) |> # remove index_id
          set_id_var(id = "id")
      }
    )
  }
)
