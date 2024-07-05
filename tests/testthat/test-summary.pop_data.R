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
  # Define the path to the RDS file in the fixtures directory
  summary_country_path <- test_path("fixtures", "summary_country.rds")

  # Load the RDS file
  summary_country <- readRDS(summary_country_path)


  expect_equal(object = xs_data %>%
    summary(strata = "Country"), expected = summary_country)
})
