test_that("message is produced when cores are reduced", {
  check_parallel_cores(Inf) |>
    expect_message(class = "reduced num_cores")
})
