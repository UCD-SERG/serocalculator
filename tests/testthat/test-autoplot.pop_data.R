test_that("`autoplot.pop_data()` raise
          an error when unavailable type is provided",
          {
            xs_data <- sees_pop_data_pk_100

            expect_error(object = xs_data |>
                           autoplot(strata = "catchment", type = "den"))
          })

test_that("`autoplot.pop_data()` raise
          an error when unavailable `strata` is provided",
          {
            xs_data <- sees_pop_data_pk_100

            expect_error(object = xs_data |>
                           autoplot(strata = "strat1", type = "density"))
          })

test_that("`autoplot.pop_data()` produces
          stable results for `type = 'density'`",
          {
            skip_if(getRversion() < "4.4.1") # 4.3.3 had issues
            xs_data <- sees_pop_data_pk_100 |>
              autoplot(strata = "catchment", type = "density") |>
              vdiffr::expect_doppelganger(title = "density")

            xs_data <- sees_pop_data_pk_100 |>
              autoplot(strata = "catchment",
                       type = "density",
                       log = TRUE) |>
              vdiffr::expect_doppelganger(title = "density-log")
          })

test_that("`autoplot.pop_data()` produces stable results for
          `type = 'age-scatter'`",
          {

            xs_data <- sees_pop_data_pk_100 |>
              autoplot(strata = "catchment", type = "age-scatter") |>
              vdiffr::expect_doppelganger(title = "age_scatter_strat_country")
          })

test_that("`autoplot.pop_data()` produces stable results
          for `type = 'age-scatter', strata = NULL`",
          {
            xs_data <- sees_pop_data_pk_100 |>
              autoplot(strata = NULL, type = "age-scatter") |>
              vdiffr::expect_doppelganger(title = "age_scatter_no_strat")
          })
