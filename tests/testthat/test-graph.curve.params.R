test_that("results are consistent", {
   curve <-
     typhoid_curves_nostrat_100 |>
     dplyr::filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

   plot1 <- graph.curve.params(curve)

   plot1 |> vdiffr::expect_doppelganger(title = "curve-quantiles")
})
