xs_data <- load_pop_data(
  file_path = "https://osf.io/download//n6cp3/",
  age = "Age",
  value = "result",
  id = "index_id",
  standardize = TRUE
)

test_that("`summary.pop_data()` produces an error when wrong stratification is provided", {
  expect_error(
    object = xs_data %>% summary(strata = "province"),
    regexp = "Element `province` doesn't exist.",
    fixed = TRUE
  )
})

test_that("`summary.pop_data()` does not produce an error when NULL", {
  # suppress warning avoids a deprecation tidyverse warning on use of select(data)
  suppressWarnings({
    expect_no_error(object = xs_data %>% summary(strata = NULL))
  })
})

test_that("`summary.pop_data()` does not produce an error when stratified", {
  # suppress warning avoids a deprecation tidyverse warning on use of select(data)
  suppressWarnings({
    expect_no_error(object = xs_data %>% summary(strata = "Country"))
  })
})


test_that("`summary.pop_data()` expected same results", {
  # suppress warning avoids a deprecation tidyverse warning on use of select(data)
  suppressWarnings({
    expect_equal(object = xs_data %>%
      summary(strata = "Country"), expected = summary_country)
  })
})
