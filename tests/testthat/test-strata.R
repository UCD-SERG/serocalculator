test_that("results are consistent", {

  sees_typhoid_ests_strat |>
    strata() |>
    expect_snapshot_data(name = "strata-ests")


})
