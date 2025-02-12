test_that(
  desc = "check for uneven counts works",
  code = {

  sees_pop_data_pk_100 |>
    tail(-1) |>
      count_strata(strata_varnames = "catchment") |>
      expect_warning(class = "incomplete-obs")

})
