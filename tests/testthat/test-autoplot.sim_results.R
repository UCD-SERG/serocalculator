test_that("results are consistent", {

  test_sim_results <-
    test_path("fixtures", "test_sim_results.rds") |>
    readr::read_rds()
  test_sim_results |>
    analyze_sims() |>
    autoplot() |>
    vdiffr::expect_doppelganger(title = "autoplot-sim-results")



})

test_that("x_var, group_var, color_var arguments work", {

  sim_results <-
    test_path("fixtures", "test_sim_results.rds") |>
    readr::read_rds() |>
    analyze_sims()

  # swap x and group axes
  p <- autoplot(
    sim_results,
    x_var = "lambda.sim",
    group_var = "sample_size",
    color_var = "sample_size"
  )

  expect_s3_class(p, "ggplot")

  vdiffr::expect_doppelganger(
    title = "autoplot-sim-results-swapped-axes",
    fig = p
  )
})

test_that("color_var defaults to group_var", {

  sim_results <-
    test_path("fixtures", "test_sim_results.rds") |>
    readr::read_rds() |>
    analyze_sims()

  p_default_color <- autoplot(
    sim_results,
    x_var = "lambda.sim",
    group_var = "sample_size"
  )

  p_explicit_color <- autoplot(
    sim_results,
    x_var = "lambda.sim",
    group_var = "sample_size",
    color_var = "sample_size"
  )

  expect_equal(p_default_color, p_explicit_color)
})
