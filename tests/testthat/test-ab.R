test_that("`ab()` works consistently", {

  par1 <- matrix(
    c(
      1.11418923843475, 1, 0.12415057798022207, 0.24829344792968783,
      0.01998946878312856, 0.0012360802436587237, 1.297194045996013,
      1.3976510415108334, 1, 0.2159993563893431, 0.4318070551383313,
      0.0015146395107173347, 0.0003580062906750277, 1.5695811573082081
    ),
    nrow = 7L,
    ncol = 2L,
    dimnames = list(
      params = c("y0", "b0", "mu0", "mu1", "c1", "alpha", "shape_r"),
      antigen_iso = c("HlyE_IgA", "HlyE_IgG")
    )
  )
  t <- 0:1444
  blims <- matrix(
    rep(c(0, 0.5), each = 2L),
    nrow = 2L,
    ncol = 2L,
    dimnames = list(c("HlyE_IgA", "HlyE_IgG"), c("min", "max"))
  )
  preds <- ab(t = t, par = par1, blims = blims)

  colnames(preds) <- colnames(par1)

  preds2 <- preds |> as_tibble()

  expect_snapshot_data(preds2, name = "ab-preds")

})
