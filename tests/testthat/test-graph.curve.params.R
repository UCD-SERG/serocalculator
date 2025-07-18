test_that(
  desc = "results are consistent",
  code = {
    curve <- typhoid_curves_nostrat_100 |>
      dplyr::filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) |>
      dplyr::mutate(.by = antigen_iso, chain = rep(1:2, times = n() / 2))

    # 1. Default quantiles: c(0.1, 0.5, 0.9)
    plot1 <- graph.curve.params(
      curve,
      n_curves = 0
    )
    plot1 |> vdiffr::expect_doppelganger(title = "curve-quantiles")

    # 2. Default quantiles + all MCMC samples
    plot2 <- graph.curve.params(
      curve,
      n_curves = Inf
    )
    plot2 |> vdiffr::expect_doppelganger(title = "curve-quantiles-and-samples")

    # 3. Test that disabling quantiles works correctly (curves only)
    plot3 <- graph.curve.params(
      curve,
      n_curves = Inf,
      quantiles = NULL
    )

    plot3 |> vdiffr::expect_doppelganger(title = "curve-samples")

    # 4. Test that custom numeric quantiles are drawn correctly
    plot4 <- graph.curve.params(
      curve,
      n_curves = 0,
      quantiles = c(0.05, 0.55, 0.95)
    )
    plot4 |> vdiffr::expect_doppelganger(title = "curve-custom-quantiles")

    # 5. Test that chain_color = FALSE works correctly
    plot5 <- graph.curve.params(
      curve,
      n_curves = Inf,
      quantiles = c(0.05, 0.55, 0.95),
      chain_color = FALSE
    )
    plot5 |> vdiffr::expect_doppelganger(title = "curve-black-chains")
  }
)

test_that(
  desc = "results are consistent with log_x",
  code = {
    curve <-
      typhoid_curves_nostrat_100 |>
      dplyr::filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) |>
      dplyr::mutate(.by = antigen_iso, chain = rep(1:2, times = n() / 2))

    # Test that log_x argument works with samples only
    plot3 <- graph.curve.params(
      curve,
      n_curves = Inf,
      log_x = TRUE
    )
    plot3 |>
      vdiffr::expect_doppelganger(title = "curve-samples-log_x")
  }
)
