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

  ssdtools:::expect_snapshot_data(csdata, "sim_pop_data")
})
