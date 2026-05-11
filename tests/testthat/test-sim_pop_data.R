test_that("`sim_pop_data()` produces consistent results", {
  # Load curve parameters
  curve <-
    typhoid_curves_nostrat_100

  # Specify the antibody-isotype responses to include in analyses
  antibodies <- c("HlyE_IgA", "HlyE_IgG")

  # Set seed to reproduce results
  set.seed(54321)

  # Simulated incidence rate per person-year
  lambda <- 0.2

  # Range covered in simulations
  lifespan <- c(0, 10)

  # Cross-sectional sample size
  nrep <- 100

  # Biologic noise distribution
  dlims <- rbind(
    "HlyE_IgA" = c(min = 0, max = 0.5),
    "HlyE_IgG" = c(min = 0, max = 0.5)
  )

  # Generate cross-sectional data
  csdata <- sim_pop_data(
    curve_params = curve,
    lambda = lambda,
    n.smpl = nrep,
    age.rng = lifespan,
    antigen_isos = antibodies,
    n.mc = 0,
    renew.params = TRUE,
    add.noise = TRUE,
    noise_limits = dlims,
    format = "long"
  )

  expect_snapshot_data(csdata, name = "sim_pop_data")
})

test_that("`sim_pop_data()` accepts numeric verbose levels", {
  curve <- typhoid_curves_nostrat_100
  antibodies <- c("HlyE_IgA", "HlyE_IgG")
  dlims <- rbind(
    "HlyE_IgA" = c(min = 0, max = 0.5),
    "HlyE_IgG" = c(min = 0, max = 0.5)
  )

  base_args <- list(
    curve_params = curve,
    lambda = 0.2,
    n_samples = 8,
    age_range = c(0, 2),
    antigen_isos = antibodies,
    n_mcmc_samples = 10,
    renew_params = TRUE,
    add_noise = FALSE,
    noise_limits = dlims,
    format = "wide"
  )

  messages_0 <- capture_messages(
    do.call(sim_pop_data, c(base_args, list(verbose = 0)))
  )
  expect_length(messages_0, 0)

  messages_1 <- capture_messages(
    do.call(sim_pop_data, c(base_args, list(verbose = 1)))
  )
  expect_true(
    any(grepl("outputting wide format data", messages_1, fixed = TRUE))
  )
  expect_false(
    any(grepl("inputs to `sim_pop_data()`:", messages_1, fixed = TRUE))
  )

  messages_2 <- capture_messages(
    do.call(sim_pop_data, c(base_args, list(verbose = 2)))
  )
  expect_true(
    any(grepl("inputs to `sim_pop_data()`:", messages_2, fixed = TRUE))
  )
  expect_true(
    any(grepl("outputting wide format data", messages_2, fixed = TRUE))
  )
})
