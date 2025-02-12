test_that(
  desc = "results are consistent",
  code = {
    curve <-
      typhoid_curves_nostrat_100 |>
      dplyr::filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) |>
      dplyr::mutate(.by = antigen_iso, chain = rep(1:2, times = n() / 2))
    plot1 <- graph.curve.params(curve)
    plot1 |> vdiffr::expect_doppelganger(title = "curve-quantiles")
    plot2 <- graph.curve.params(curve, show_all_curves = TRUE)
    plot2 |>
      vdiffr::expect_doppelganger(title = "curve-quantiles-and-samples")
  }
)
