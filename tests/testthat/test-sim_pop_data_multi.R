test_that("`sim_pop_data_multi()` works consistently", {
  skip_on_cran()
  # Load curve parameters
  dmcmc <- typhoid_curves_nostrat_100

  # Specify the antibody-isotype responses to include in analyses
  antibodies <- c("HlyE_IgA", "HlyE_IgG")

  # Set seed to reproduce results
  set.seed(54321)

  # Simulated incidence rate per person-year
  lambdas <- c(.05, .1, .15, .2, .3)
  # Range covered in simulations
  lifespan <- c(0, 10)

  # Cross-sectional sample size
  sample_sizes <- c(100, 50)

  # Biologic noise distribution
  dlims <- rbind(
    "HlyE_IgA" = c(min = 0, max = 0.5),
    "HlyE_IgG" = c(min = 0, max = 0.5)
  )

  pop_data_multi <- sim_pop_data_multi(
    curve_params = dmcmc,
    lambdas = lambdas,
    sample_sizes = sample_sizes,
    age_range = lifespan,
    antigen_isos = antibodies,
    n_mcmc_samples = 0,
    renew_params = TRUE,
    add_noise = TRUE,
    noise_limits = dlims,
    format = "long",
    nclus = 10
  )

  pop_data_multi |>
    expect_snapshot_data(name = "pop_data_multi", digits = 3)
})

test_that("`sim_pop_data_multi()` can dispatch to `sim_pop_data_2()`", {
  skip_on_cran()
  dmcmc <- typhoid_curves_nostrat_100
  antibodies <- c("HlyE_IgA", "HlyE_IgG")

  set.seed(54321)

  # nclus = 1 (with a single lambda/sample_size) is avoided here: it makes
  # `n_sample_sizes * n_lambda * nclus == 1`, which hits a pre-existing
  # `sim_pop_data_multi()` bug where `rngtools::RNGseq(1, seed)` returns an
  # unwrapped 7-integer state vector (instead of a list-of-one), silently
  # truncated to its first element by the subsequent `array()` call -- this
  # feeds `rngtools::setRNG()` an invalid state and makes results
  # non-reproducible across runs. Tracked in #554; nclus = 2 sidesteps it.
  pop_data_multi_2 <- sim_pop_data_multi(
    sim_function = sim_pop_data_2,
    curve_params = dmcmc,
    lambdas = 0.2,
    sample_sizes = 5,
    age_range = c(0, 10),
    antigen_isos = antibodies,
    n_mcmc_samples = 0,
    renew_params = TRUE,
    add_noise = FALSE,
    format = "long",
    nclus = 2
  )

  pop_data_multi_2 |>
    expect_snapshot_data(name = "pop_data_multi_2", digits = 3)
})

test_that("`sim_pop_data_multi()` verbose = TRUE prints inputs w/o erroring", {
  dmcmc <- typhoid_curves_nostrat_100

  expect_message(
    sim_pop_data_multi(
      curve_params = dmcmc,
      lambdas = 0.2,
      sample_sizes = 5,
      age_range = c(0, 10),
      antigen_isos = c("HlyE_IgA", "HlyE_IgG"),
      n_mcmc_samples = 0,
      add_noise = FALSE,
      noise_limits = rbind(
        "HlyE_IgA" = c(min = 0, max = 0.5),
        "HlyE_IgG" = c(min = 0, max = 0.5)
      ),
      format = "long",
      nclus = 2,
      verbose = TRUE
    )
  )
})

test_that("`sim_pop_data_multi()` leaves the caller's RNG state unchanged", {
  # `rngtools::setRNG()` inside the `%dopar%` loop switches the generator to
  # L'Ecuyer-CMRG. With `num_cores = 1` on Unix, `doParallel` registers a
  # multicore backend that runs the loop in the calling process, so that
  # switch used to escape into the user's session: a later `set.seed()` then
  # produced a different stream than it had before the call.
  #
  # Note this guard is Unix-specific by construction. On Windows,
  # `registerDoParallel(cores = 1)` builds a one-worker PSOCK cluster, the
  # loop body runs out of process, and the caller's generator is never
  # touched -- so a green run there says nothing about the regression.
  dmcmc <- typhoid_curves_nostrat_100

  run_sim <- function() {
    sim_pop_data_multi(
      curve_params = dmcmc,
      lambdas = 0.2,
      sample_sizes = 5,
      age_range = c(0, 10),
      antigen_isos = c("HlyE_IgA", "HlyE_IgG"),
      n_mcmc_samples = 0,
      add_noise = FALSE,
      format = "long",
      nclus = 2,
      num_cores = 1
    )
  }

  kind_before <- RNGkind()
  set.seed(54321)
  seed_before <- .Random.seed

  invisible(run_sim())

  expect_identical(.Random.seed, seed_before)

  # The user-visible consequence: an identical `set.seed()` must still give
  # an identical stream after the call.
  set.seed(99)
  before <- runif(3)
  invisible(run_sim())
  set.seed(99)
  after <- runif(3)
  expect_identical(before, after)

  # The fresh-session case, where there is no `.Random.seed` to restore.
  # `rngtools::RNGseed()` returns NULL there and `RNGseed(NULL)` only
  # removes `.Random.seed`, so restoring the seed alone leaves the kind
  # switched. This is the branch that needs the kind captured separately.
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    rm(".Random.seed", envir = globalenv())
  }
  invisible(run_sim())
  expect_identical(RNGkind(), kind_before)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})
