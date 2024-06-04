xs_data <- load_pop_data(
  file_path = "https://osf.io/download//n6cp3/",
  age = "Age",
  value = "result",
  id = "index_id",
  standardize = TRUE
)

unstratified_summary <- xs_data %>%
  summary() %>%
  magrittr::extract2("age_summary")

test_that("`summary.pop_data()` produces same results when stratified", {
  min_country <- xs_data %>%
    summary(strata = "Country") %>%
    magrittr::extract2("age_summary") %>%
    pull(age_min) %>%
    min()

  expect_equal(object = unstratified_summary %>% pull(age_min), expected = min_country)
})

test_that("`summary.pop_data()` produces same results when stratified", {
  first_quartile_country <- xs_data %>%
    summary(strata = "Country") %>%
    magrittr::extract2("age_summary") %>%
    pull(age_first_quartile) %>%
    mean()

  expect_equal(
    unstratified_summary %>% pull(age_first_quartile) %>% as.numeric(),
    first_quartile_country,
    tolerance = 0.5
  )
})

test_that("`summary.pop_data()` produces same results when stratified", {
  median_country <- xs_data %>%
    summary(strata = "Country") %>%
    magrittr::extract2("age_summary") %>%
    pull(age_median) %>%
    mean()

  expect_equal(
    unstratified_summary %>% pull(age_median),
    median_country,
    tolerance = 0.5
  )
})

test_that("`summary.pop_data()` produces same results when stratified", {
  mean_country <- xs_data %>%
    summary(strata = "Country") %>%
    magrittr::extract2("age_summary") %>%
    pull(age_mean) %>%
    mean()

  expect_equal(
    unstratified_summary %>% pull(age_mean),
    mean_country,
    tolerance = 0.5
  )
})

test_that("`summary.pop_data()` produces same results when stratified", {
  third_quartile_country <- xs_data %>%
    summary(strata = "Country") %>%
    magrittr::extract2("age_summary") %>%
    pull(age_mean) %>%
    mean()

  expect_equal(
    unstratified_summary %>% pull(age_third_quartile) %>% as.numeric(),
    third_quartile_country,
    tolerance = 0.5
  )
})

test_that("`summary.pop_data()` produces same results when stratified", {
  max_country <- xs_data %>%
    summary(strata = "Country") %>%
    magrittr::extract2("age_summary") %>%
    pull(age_max) %>%
    mean()

  expect_equal(
    unstratified_summary %>% pull(age_max),
    max_country,
    tolerance = 0.5
  )
})

