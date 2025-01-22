test_that("`load_pop_data()` produces expected results", {
  xs_data_true <- load_pop_data(serocalculator_example("example_pop_data.rds"),
    standardize = TRUE
  )

  xs_data_false <- load_pop_data(serocalculator_example("example_pop_data.rds"),
    standardize = FALSE
  )

  true_col_names <- c("id", "Country", "cluster", "catchment", "age", "ageCat", "antigen_iso", "value")
  false_col_names <- c("index_id", "Country", "cluster", "catchment", "Age", "ageCat", "antigen_iso", "result")

  expect_equal(xs_data_true %>% names(), true_col_names)
  expect_equal(xs_data_false %>% names(), false_col_names)
})
