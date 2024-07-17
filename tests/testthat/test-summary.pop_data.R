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

test_that("`summary.pop_data()` produces stable results when `strata = NULL`", {
  expect_snapshot({
    warnings <- character()

    result <- withCallingHandlers(

      xs_data %>% summary(strata = NULL),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    # Combine result and warnings for the snapshot
    list(result = result, warnings = warnings)
  })
})

test_that("`summary.pop_data()` produces stable results with stratification", {
  expect_snapshot(xs_data %>% summary(strata = "Country"))
})
