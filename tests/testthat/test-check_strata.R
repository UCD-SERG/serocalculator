test_that(
  "`check_strata()` throws an error when elements that don't exactly
          match the columns of `pop_data` are provided",
  {
    sees_pop_data_pk_100 |>
      check_strata(strata = c("ag", "catch", "Count")) |>
      expect_error(class = "missing_var")
  }
)

test_that("`check_strata()` throws an error when `strata` is not a `character`",
          {
            sees_pop_data_pk_100 |>
              check_strata(strata = c(1,4)) |>
              expect_error(class = "strata are not strings")
          })
