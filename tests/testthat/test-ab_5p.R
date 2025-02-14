test_that("results are consistent without units", {
  params <- typhoid_curves_nostrat_100[1, ]
  ab_5p(
    t = 50,
    y0 = params$y0,
    y1 = params$y1,
    t1 = params$t1,
    alpha = params$alpha,
    shape = params$r) |>
    expect_snapshot_value(style = "deparse")
})
