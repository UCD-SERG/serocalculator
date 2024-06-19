xs_data <- load_pop_data(
  file_path = "https://osf.io/download//n6cp3/",
  age = "Age",
  value = "result",
  id = "index_id",
  standardize = TRUE
)

test_that("`summary.pop_data()` produces an error when wrong stratification is provied", {
  expect_error(object = xs_data %>% summary(strata = "province"))
  # add condition for exact error - use regexp option in expect_error()
})

test_that("`summary.pop_data()` does not produce an error when NULL", {
  suppressWarnings({
    expect_no_error(object = xs_data %>% summary(strata = NULL))
  })
})

test_that("`summary.pop_data()` does not produce an error when stratified", {

  suppressWarnings({
    expect_no_error(object = xs_data %>% summary(strata = "Country"))
  })
})

test_that("`summary.pop_data()` expected same results", {
  suppressWarnings({
    expect_equal(object = xs_data %>%
      summary(strata = "Country"), expected = summary_country)
  })
})
