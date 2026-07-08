test_that("results are consistent", {

  test_sim_results <-
    test_path("fixtures", "test_sim_results.rds") |>
    readr::read_rds()
  test_sim_results |>
    analyze_sims() |>
    autoplot() |>
    vdiffr::expect_doppelganger(title = "autoplot-sim-results")



})
