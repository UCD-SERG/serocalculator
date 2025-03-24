test_that("`test_missing_strata()` errors on absent strata", {
  expected_strata <- data.frame(Country = "US")

  withr::local_options(width = 80)
  sees_pop_data_pk_100 |>
    warn_missing_strata(
      strata = expected_strata,
      dataname = "sees_pop_data_pk_100"
    ) |>
    expect_snapshot(error = TRUE,
                    cnd_class = TRUE)
})

test_that("`test_missing_strata()` warns on missing strata vars", {
  expected_strata <- data.frame(
    Country = "Pakistan",
    place = "davis"
  )

  withr::local_options(width = 80)
  sees_pop_data_pk_100 |>
    warn_missing_strata(
      strata = expected_strata,
      dataname = "sees_pop_data_pk_100"
    ) |>
    expect_snapshot(cnd_class = TRUE)

})
