test_that("`get_biomarker_names_var()` works", {
  biomarker_names_var <-
    serocalculator_example("example_pop_data.rds") |>
    load_pop_data() |>
    get_biomarker_names_var()

  expect_equal(object = biomarker_names_var, expected = "antigen_iso")

})

test_that(
  desc =
    "`get_biomarker_names_var()` warns when attribute not set",
  code = {

    serocalculator_example("example_pop_data.rds") |>
      readr::read_rds() |>
      get_biomarker_names_var() |>
      expect_warning(class = "biomarker_var attribute missing")
  }
)

test_that(
  desc =
    "`get_biomarker_names_var()` aborts when attribute not set or
      findable",
  code = {

    iris |>
      get_biomarker_names_var() |>
      expect_error(class = "biomarker_var attribute missing")

  }
)
