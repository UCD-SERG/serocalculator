test_that(
  desc = "results are consistent",
  code = {
    ests_summary <-
      test_path("fixtures", "test_sim_results.rds") |>
      readr::read_rds()

    ests_summary |>
      filter(
        lambda.sim == 0.05,
        sample_size == 50
      ) |>
      analyze_sims(
        true_lambda = ests_summary$lambda.sim[1]) |>
      ssdtools:::expect_snapshot_data(name = "sim_results")



  }
)
