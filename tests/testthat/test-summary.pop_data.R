xs_data <- load_pop_data(
  file_path = "https://osf.io/download//n6cp3/",
  age = "Age",
  value = "result",
  id = "index_id",
  standardize = TRUE
)

test_that("`summary.pop_data()` produces an error when wrong stratification is provied", {
  expect_error(object = xs_data %>% summary(strata = "province"))
})

test_that("`summary.pop_data()` does not produce an error when NULL", {
  expect_no_error(object = xs_data %>% summary(strata = NULL))
})

test_that("`summary.pop_data()` does not produce an error when stratified", {
  expect_no_error(object = xs_data %>% summary(strata = "Country"))
})

# compare outputs
test_that("`summary.pop_data()` expected", {
  skip(message = "skipping to fix file access iisue")
  country <-
    fs::path_package(
      "extdata",
      "country.rda",
      package = "serocalculator"
    ) %>%
    load()
    #readRDS()

  gen_country <- xs_data %>%
    summary(strata = "Country") %>%
    magrittr::extract2("age_summary")

  expect_equal(object = gen_country,expected = country)
})


