test_that("`plot_decay_curve()` returns a ggplot object", {
  p <- plot_decay_curve(antibody_decay_curve)
  expect_s3_class(p, "ggplot")
})

test_that("`plot_decay_curve()` works with `pathogen_decay_curve()`", {
  p <- plot_decay_curve(pathogen_decay_curve)
  expect_s3_class(p, "ggplot")
})

test_that("`plot_decay_curve()` respects the `xmax` parameter", {
  p_default <- plot_decay_curve(antibody_decay_curve)
  p_xmax <- plot_decay_curve(antibody_decay_curve, xmax = 50)
  built_default <- ggplot2::ggplot_build(p_default)
  built_xmax <- ggplot2::ggplot_build(p_xmax)

  expect_lt(
    built_xmax$layout$panel_params[[1]]$x.range[2],
    built_default$layout$panel_params[[1]]$x.range[2]
  )
})

test_that("`plot_decay_curve()` respects the `ymax` parameter", {
  p_default <- plot_decay_curve(antibody_decay_curve)
  p_ymax <- plot_decay_curve(antibody_decay_curve, ymax = 1e6)
  built_default <- ggplot2::ggplot_build(p_default)
  built_ymax <- ggplot2::ggplot_build(p_ymax)

  expect_gt(
    built_ymax$layout$panel_params[[1]]$y.range[2],
    built_default$layout$panel_params[[1]]$y.range[2]
  )
})

test_that("`plot_decay_curve()` sets the plot title", {
  p <- plot_decay_curve(antibody_decay_curve, title = "my title")
  expect_equal(p$labels$title, "my title")
})
