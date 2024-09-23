test_that("`autoplot.pop_data()` raise
          an error when unavailable type is provided",
          {
            xs_data <- load_pop_data(
              file_path = "https://osf.io/download//n6cp3/",
              age = "Age",
              id = "index_id",
              value = "result",
              standardize = TRUE
            )
            expect_error(object = xs_data %>%
                           autoplot(strata = "Country", type = "den"))
          })

test_that("`autoplot.pop_data()` raise
          an error when unavailable `strata` is provided",
          {
            xs_data <- load_pop_data(
              file_path = "https://osf.io/download//n6cp3/",
              age = "Age",
              id = "index_id",
              value = "result",
              standardize = TRUE
            )
            expect_error(object = xs_data %>%
                           autoplot(strata = "strat1", type = "density"))
          })

test_that("`autoplot.pop_data()` produces stable results for `type = 'density'`",
          {
            xs_data <- load_pop_data(
              file_path = "https://osf.io/download//n6cp3/",
              age = "Age",
              id = "index_id",
              value = "result",
              standardize = TRUE
            ) %>%
              autoplot(strata = "Country", type = "density") %>%
              ggplot2::ggsave(
                filename = tempfile(),
                device = "svg",
                width = 8,
                height = 8
              ) %>%
              expect_snapshot_file(name = "density.svg")
          })

test_that("`autoplot.pop_data()` produces stable results for
          `type = 'age-scatter'`",
          {
            xs_data <- load_pop_data(
              file_path = "https://osf.io/download//n6cp3/",
              age = "Age",
              id = "index_id",
              value = "result",
              standardize = TRUE
            ) %>%
              autoplot(strata = "Country", type = "age-scatter") %>%
              ggplot2::ggsave(
                filename = tempfile(),
                device = "svg",
                width = 8,
                height = 8
              ) %>%
              expect_snapshot_file(name = "age_scatter_strat_country.svg")
          })

test_that("`autoplot.pop_data()` produces stable results
          for `type = 'age-scatter' no strat`",
          {
            xs_data <- load_pop_data(
              file_path = "https://osf.io/download//n6cp3/",
              age = "Age",
              id = "index_id",
              value = "result",
              standardize = TRUE
            ) %>%
              autoplot(strata = NULL, type = "age-scatter") %>%
              ggplot2::ggsave(
                filename = tempfile(),
                device = "svg",
                width = 8,
                height = 8
              ) %>%
              expect_snapshot_file(name = "age_scatter_no_strat.svg")
          })
