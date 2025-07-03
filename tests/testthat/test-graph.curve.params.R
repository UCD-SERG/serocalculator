test_that(
  desc = "results are consistent",
  code = {
    curve <-
      typhoid_curves_nostrat_100 |>
      dplyr::filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) |>
      dplyr::mutate(.by = antigen_iso, chain = rep(1:2, times = n() / 2))
    plot1 <- graph.curve.params(
      curve,
      show_quantiles = TRUE,
      n_curves = 0
    )
    plot1 |> vdiffr::expect_doppelganger(title = "curve-quantiles")
    plot2 <- graph.curve.params(
      curve,
      show_quantiles = TRUE,
      n_curves = Inf
    )
    plot2 |>
      vdiffr::expect_doppelganger(title = "curve-quantiles-and-samples")
    plot3 <- graph.curve.params(
      curve,
      n_curves = Inf,
      show_quantiles = FALSE
    )
    plot3 |>
      vdiffr::expect_doppelganger(title = "curve-samples")
  }
)

test_that(
  desc = "results are consistent with log_x",
  code = {
    curve <-
      typhoid_curves_nostrat_100 |>
      dplyr::filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) |>
      dplyr::mutate(.by = antigen_iso, chain = rep(1:2, times = n() / 2))
    plot3 <- graph.curve.params(
      curve,
      n_curves = Inf,
      show_quantiles = TRUE,
      log_x = TRUE
    )
    plot3 |>
      vdiffr::expect_doppelganger(title = "curve-samples-log_x")
  }
)
