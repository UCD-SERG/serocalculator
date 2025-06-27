test_that(
  desc = "results are consistent",
  code = {
    test_sim_results <-
      test_path("fixtures", "test_sim_results.rds") |>
      readr::read_rds()

    test_sim_results |>
      analyze_sims() |>
      ssdtools:::expect_snapshot_data(name = "sim_results")

  }
)
