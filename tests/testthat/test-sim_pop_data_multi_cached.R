noise_limits_2 <- rbind(
  "HlyE_IgA" = c(min = 0, max = 0.5),
  "HlyE_IgG" = c(min = 0, max = 0.5)
)

sim_small <- function(cache_path, ...) {
  # `sim_pop_data_multi()` switches the session's RNG to L'Ecuyer-CMRG via
  # `rngtools::RNGseq()` and does not switch it back, which would change the
  # random draws every later test file makes and break their snapshots.
  withr::local_preserve_seed()

  sim_pop_data_multi_cached(
    curve_params = typhoid_curves_nostrat_100,
    lambdas = 0.1,
    nclus = 2,
    sample_sizes = 20,
    age_range = c(0, 10),
    antigen_isos = c("HlyE_IgA", "HlyE_IgG"),
    num_cores = 1,
    add_noise = TRUE,
    noise_limits = noise_limits_2,
    format = "long",
    cache_path = cache_path,
    cache_verbose = FALSE,
    ...
  )
}

test_that("`sim_pop_data_multi_cached()` returns what it wraps", {
  dir <- withr::local_tempdir()
  result <- sim_small(dir)

  expect_s3_class(result, "pop_data")
  expect_equal(nrow(result), 80)
})

test_that("`sim_pop_data_multi_cached()` reuses its cache", {
  dir <- withr::local_tempdir()

  first <- sim_small(dir)
  second <- sim_small(dir)

  expect_identical(first, second)
  expect_true(dir.exists(file.path(dir, "sim_pop_data_multi")))
})

test_that("`sim_pop_data_multi_cached()` recomputes on `cache_rerun`", {
  dir <- withr::local_tempdir()

  first <- sim_small(dir)
  # A different `rng_seed` guarantees different data, so an unchanged result
  # would prove the cache was consulted when it should not have been.
  rerun <- sim_small(dir, rng_seed = 99, cache_rerun = TRUE)

  expect_false(identical(first, rerun))
})

test_that("`cache_verbose` does not swallow the wrapped `verbose`", {
  # `sim_pop_data_multi()` has its own `verbose` argument. The cache-control
  # arguments are prefixed precisely so a caller can still reach it.
  dir <- withr::local_tempdir()

  expect_message(
    sim_small(dir, verbose = TRUE),
    "inputs to"
  )
})
