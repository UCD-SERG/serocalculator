xs_data <-
  sees_pop_data_pk_100

test_that("`summary.pop_data()` produces an error when wrong stratification is provided", {
  expect_error(
    object = xs_data %>% summary(strata = "province"),
    regexp = "Element `province` doesn't exist.",
    fixed = TRUE
  )
})

test_that("`summary.pop_data()` produces stable results when `strata = NULL`", {
  expect_snapshot(xs_data %>% summary(strata = NULL))
})

test_that("`summary.pop_data()` produces stable results with stratification", {
  expect_snapshot(xs_data %>% summary(strata = "catchment"))
})
