# Load curve parameters
dmcmc <- typhoid_curves_nostrat_100

# Specify the antibody-isotype responses to include in analyses
antibodies <- c("HlyE_IgA", "HlyE_IgG")

# Set seed to reproduce results
set.seed(54321)

# Simulated incidence rate per person-year
lambdas = c(.05, .1, .15, .2, .3)

# Range covered in simulations
lifespan <- c(0, 10);

# Cross-sectional sample size
nrep <- 100

# Biologic noise distribution
dlims <- rbind(
  "HlyE_IgA" = c(min = 0, max = 0.5),
  "HlyE_IgG" = c(min = 0, max = 0.5)
)

sim_data <- sim_pop_data_multi(
  curve_params = dmcmc,
  lambdas = lambdas,
  sample_sizes = nrep,
  age_range = lifespan,
  antigen_isos = antibodies,
  n_mcmc_samples = 0,
  renew_params = TRUE,
  add_noise = TRUE,
  noise_limits = dlims,
  format = "long",
  nclus = 10)

sim_data
