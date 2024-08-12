
test_that("`load_pop_data()` produces expected results", {
  xs_data_true <- load_pop_data(
    file_path = "https://osf.io/download//n6cp3/",
    age = "Age",
    value = "result",
    id = "index_id",
    standardize = TRUE
  )

  xs_data_false <- load_pop_data(
    file_path = "https://osf.io/download//n6cp3/",
    age = "Age",
    value = "result",
    id = "index_id",
    standardize = FALSE
  )

  true_col_names <- c("id", "Country", "cluster", "catchment", "age", "ageCat", "antigen_iso", "value")
  false_col_names <- c("index_id", "Country", "cluster", "catchment", "Age", "ageCat", "antigen_iso", "result")

  expect_equal(xs_data_true %>% names(), true_col_names)
  expect_equal(xs_data_false %>% names(), false_col_names)
})

test_that("`load_pop_data()` works as expected", {

  # get pop data
  xs_data <- load_pop_data(
    file_path = "https://osf.io/download//n6cp3/",
    age = "Age",
    value = "result",
    id = "index_id",
    standardize = TRUE
  )

  expect_equal(
    object = xs_data %>% get_id(),
    expected = xs_data$id
  )

  # `load_pop_data()` checks biomarker in `pop_data` columns
  expect_error(
    object = xs_data %>% set_biomarker_var(biomarker = "antigen"),
    class = "no biomarker"
  )

  # `load_pop_data()` gets biomarker levels
  expect_equal(
    object = xs_data %>% get_biomarker_levels(),
    expected = unique(xs_data$antigen_iso)
  )

  # `load_pop_data()` gets biomarker attribute names
  expect_equal(
    object = xs_data %>% get_biomarker_names_var(),
    expected = xs_data %>% select(antigen_iso) %>% names()
  )

})

