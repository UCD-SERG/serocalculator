# warns when specified id not found

    Code
      xs_data <- set_id_var(readr::read_rds(serocalculator_example(
        "example_pop_data.rds")), id = "id")
    Condition <rlang_warning>
      Warning:
      The specified `id` column "id" does not exist.
      i Proceeding to use "index_id"

# aborts when specified id not found and no partial match found

    Code
      xs_data <- set_id_var(dplyr::select(readr::read_rds(serocalculator_example(
        "example_pop_data.rds")), -index_id), id = "id")
    Condition <rlang_error>
      Error in `set_id_var()`:
      ! The specified `id` column "id" does not exist.
      x No similar column name was detected.

