test_that("`autoplot.pop_data()` raise
          an error when unavailable type is provided",
          {
            xs_data <- load_pop_data(serocalculator_example("example_pop_data.rds")
            )
            expect_error(object = xs_data %>%
                           autoplot(strata = "catchment", type = "den"))
          })

test_that("`autoplot.pop_data()` raise
          an error when unavailable `strata` is provided",
          {
            xs_data <- load_pop_data(serocalculator_example("example_pop_data.rds")
            )
            expect_error(object = xs_data %>%
                           autoplot(strata = "strat1", type = "density"))
          })

test_that("`autoplot.pop_data()` produces
          stable results for `type = 'density'`",
          {
            skip_if(getRversion() < "4.4.1") # 4.3.3 had issues
            xs_data <- load_pop_data(serocalculator_example("example_pop_data.rds")
            ) %>%
              autoplot(strata = "catchment", type = "density") %>%
              vdiffr::expect_doppelganger(title = "density")
          })

test_that("`autoplot.pop_data()` produces stable results for
          `type = 'age-scatter'`",
          {

            xs_data <- load_pop_data(serocalculator_example("example_pop_data.rds")
            ) %>%
              autoplot(strata = "catchment", type = "age-scatter") %>%
              vdiffr::expect_doppelganger(title = "age_scatter_strat_country")
          })

test_that("`autoplot.pop_data()` produces stable results
          for `type = 'age-scatter', strata = NULL`",
          {
            xs_data <- load_pop_data(serocalculator_example("example_pop_data.rds")
            ) %>%
              autoplot(strata = NULL, type = "age-scatter") %>%
              vdiffr::expect_doppelganger(title = "age_scatter_no_strat")
          })
