test_that("`simulate_pop_data()` produces consistent results", {

  # Load curve parameters
  dmcmc <- load_curve_params("https://osf.io/download/rtw5k")

  # Specify the antibody-isotype responses to include in analyses
  antibodies <- c("HlyE_IgA", "HlyE_IgG")

  # Set seed to reproduce results
  set.seed(54321)

  # Simulated incidence rate per person-year
  lambda <- 0.2;

  # Range covered in simulations
  lifespan <- c(0, 10);

  # Cross-sectional sample size
  nrep <- 100

  # Biologic noise distribution
  dlims <- rbind(
    "HlyE_IgA" = c(min = 0, max = 0.5),
    "HlyE_IgG" = c(min = 0, max = 0.5)
  )

  # Generate cross-sectional data
  csdata <- simulate_pop_data(
    curve_params = dmcmc,
    lambda = lambda,
    n.smpl = nrep,
    age.rng = lifespan,
    antigen_isos = antibodies,
    n.mc = 0,
    renew.params = TRUE,
    add.noise = TRUE,
    noise_limits = dlims,
    format = "long"
  ) %>% ssdtools:::expect_snapshot_data(name = "sim_pop_data_1")

})
