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
              check_strata(strata = c(1, 4)) |>
              expect_error(class = "strata are not strings")
          })

test_that(
  desc =
  "`check_strata()` warns when some strata of `pop_data`
  entirely lack some biomarkers",
  code = {
    sees_pop_data_100 |>
      dplyr::filter(Country == "Nepal",
                    catchment == "kavre" | antigen_iso == "HlyE_IgA") |>
    check_strata(
      strata = "catchment"
    ) |>
      expect_warning(class = "strata missing some biomarkers")
  }
)

test_that(
  desc =
    "`check_strata()` can handle when there are no partial matches",
  code = {
    sees_pop_data_100 |>
      check_strata(
        strata = "Country2"
      ) |>
      expect_error(class = "missing_var")
  }
)
