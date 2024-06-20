xs_data <- load_pop_data(
  file_path = "https://osf.io/download//n6cp3/",
  age = "Age",
  value = "result",
  id = "index_id",
  standardize = TRUE
)

test_that("`summary.pop_data()` produces an error when wrong stratification is provide", {
  expect_error(
    object = xs_data %>% summary(strata = "province"),
    regexp = "Element `province` doesn't exist.",
    fixed = TRUE
  )
})

test_that("`summary.pop_data()` does not produce an error when NULL", {
  # TO DOs: change calls to deprecated function calls (New PR)
  suppressWarnings({
    expect_no_error(object = xs_data %>% summary(strata = NULL))
  })
})

test_that("`summary.pop_data()` does not produce an error when stratified", {
  # TO DOs: change calls to deprecated function calls (New PR)
  suppressWarnings({
    expect_no_error(object = xs_data %>% summary(strata = "Country"))
  })
})


test_that("`summary.pop_data()` expected same results", {
  # TO DOs: change calls to deprecated function calls (New PR)
  suppressWarnings({
    expect_equal(object = xs_data %>%
      summary(strata = "Country"), expected = summary_country)
  })
})
