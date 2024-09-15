test_that("`autoplot.pop_data()` raise
          an error when unavailale type is provided", {
  xs_data <- load_pop_data(
    file_path = "https://osf.io/download//n6cp3/",
    age = "Age",
    id = "index_id",
    value = "result",
    standardize = TRUE
  )

  expect_error(
    object = xs_data %>%
      autoplot(strata = "Country", type = "den")
  )
})
