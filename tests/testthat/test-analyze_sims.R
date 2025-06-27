test_that(
  desc = "results are consistent",
  code = {
    ests_summary <-
      test_path("fixtures", "test_sim_results.rds") |>
      readr::read_rds()

    ests_summary |>
      dplyr::group_by(
        lambda.sim, sample_size) |>
      dplyr::group_map(~analyze_sims(.x), .keep = TRUE) |>
      bind_rows() |>
      ssdtools:::expect_snapshot_data(name = "sim_results")

  }
)
