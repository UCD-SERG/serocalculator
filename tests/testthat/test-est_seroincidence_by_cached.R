est_small <- function(pop_data, cache_path, ...) {
  est_seroincidence_by_cached(
    pop_data = pop_data,
    sr_params = serocalculator::typhoid_curves_nostrat_100,
    noise_params = serocalculator::example_noise_params_pk,
    strata = "cluster",
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
    build_graph = FALSE,
    num_cores = 1,
    verbose = FALSE,
    cache_path = cache_path,
    cache_verbose = FALSE,
    ...
  )
}

# `sim_pop_data_multi()` defaults to a fixed `rng_seed`, so it is
# deterministic: varying the seed is what makes two data sets differ.
small_pop_data <- function(rng_seed = 1234) {
  # See the note in test-sim_pop_data_multi_cached.R: this call leaves the
  # session's RNG kind changed unless it is restored here.
  withr::local_preserve_seed()

  sim_pop_data_multi(
    curve_params = serocalculator::typhoid_curves_nostrat_100,
    lambdas = 0.1,
    nclus = 2,
    sample_sizes = 20,
    age_range = c(0, 10),
    antigen_isos = c("HlyE_IgA", "HlyE_IgG"),
    num_cores = 1,
    rng_seed = rng_seed,
    add_noise = TRUE,
    noise_limits = rbind(
      "HlyE_IgA" = c(min = 0, max = 0.5),
      "HlyE_IgG" = c(min = 0, max = 0.5)
    ),
    format = "long"
  )
}

test_that("`est_seroincidence_by_cached()` returns what it wraps", {
  dir <- withr::local_tempdir()

  result <- est_small(small_pop_data(), dir)

  expect_s3_class(result, "seroincidence.by")
})

test_that("`est_seroincidence_by_cached()` reuses its cache", {
  dir <- withr::local_tempdir()
  pop_data <- small_pop_data()

  first <- est_small(pop_data, dir)
  second <- est_small(pop_data, dir)

  expect_identical(summary(first), summary(second))
  expect_true(dir.exists(file.path(dir, "est_seroincidence_by")))
})

test_that("`est_seroincidence_by_cached()` recomputes for different data", {
  dir <- withr::local_tempdir()

  first <- est_small(small_pop_data(rng_seed = 1234), dir)
  # Genuinely different simulated data must produce a different estimate, so
  # an identical summary would mean the cache ignored the `pop_data` argument.
  second <- est_small(small_pop_data(rng_seed = 99), dir)

  expect_false(identical(summary(first), summary(second)))
})
