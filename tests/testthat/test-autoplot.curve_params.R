test_that(
  desc = "results are consistent",
  code = {

    curve <-
      serocalculator_example("example_curve_params.csv") |>
      read.csv() |>
      as_curve_params() |>
      filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) |>
      autoplot()

    curve |>
      vdiffr::expect_doppelganger(
        title = "autoplot.curve_params")
  }

)
