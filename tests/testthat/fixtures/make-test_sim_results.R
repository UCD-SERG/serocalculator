dmcmc <- typhoid_curves_nostrat_100

library(parallel)
n_cores <- max(1, parallel::detectCores() - 1)

nclus <- 20
# cross-sectional sample size
nrep <- 100

# incidence rate in e
lambdas <- c(.05, .1, .15, .2, .5, .8)

sim_df <-
  sim_pop_data_multi(
    n_cores = n_cores,
    lambdas = lambdas,
    nclus = nclus,
    sample_sizes = nrep,
    age_range = lifespan,
    antigen_isos = antibodies,
    renew_params = renew_params,
    add_noise = TRUE,
    curve_params = dmcmc,
    noise_limits = dlims,
    format = "long"
  )

ests <-
  est_seroincidence_by(
    pop_data = sim_df,
    curve_params = dmcmc,
    noise_params = cond,
    num_cores = n_cores,
    strata = c("lambda.sim", "cluster"),
    curve_strata_varnames = NULL,
    noise_strata_varnames = NULL,
    verbose = verbose,
    build_graph = TRUE, # slows down the function substantially
    antigen_isos = c("HlyE_IgG", "HlyE_IgA")
  )
