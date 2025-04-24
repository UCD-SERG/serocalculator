test_that(
  desc = "results are consistent",
  code = {
    curve <- typhoid_curves_nostrat_100 |>
      dplyr::filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) |>
      dplyr::mutate(.by = antigen_iso, chain = rep(1:2, times = n() / 2))

    # 1. Default quantiles: c(0.1, 0.5, 0.9)
    plot1 <- graph.curve.params(curve, show_all_curves = FALSE)
    vdiffr::expect_doppelganger(title = "curve-quantiles", fig = plot1)

    # 2. Default quantiles + all MCMC samples
    plot2 <- graph.curve.params(curve, show_all_curves = TRUE)
    vdiffr::expect_doppelganger(title = "curve-quantiles-and-samples",
                                fig = plot2)

    # 3. Custom numeric quantiles only
    plot3 <- graph.curve.params(
      curve,
      show_all_curves = TRUE,
      quantiles = c(0.05, 0.55, 0.95)
    )
    vdiffr::expect_doppelganger(title = "curve-custom-quantiles", fig = plot3)
  }
)
