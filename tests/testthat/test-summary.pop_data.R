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
  expect_no_error(object = xs_data %>% summary(strata = NULL))
})

test_that("`summary.pop_data()` does not produce an error when stratified", {
  expect_no_error(object = xs_data %>% summary(strata = "Country"))
})


test_that("`summary.pop_data()` expected same results", {

  # load the RDS file
  sum_country <- readRDS(test_path("fixtures", "summary_country.rds"))

  expect_snapshot(x = xs_data %>% summary(strata = "Country"))
})
