#' Simulate multiple data sets
#'
#' @param nclus  number of clusters
#' @param rng_seed starting seed for random number generator,
#' passed to [rngtools::RNGseq()]
#' @param lambdas #incidence rate, in events/person*year
#' @param num_cores number of cores to use for parallel computations
#' @param verbose whether to report verbose information
#' @param ... arguments passed to [sim.cs()]
#' @inheritDotParams sim_pop_data
#' @return a [tibble::tibble()]
#' @export
#' @examples
#' # Load curve parameters
#' dmcmc <- typhoid_curves_nostrat_100
#'
#' # Specify the antibody-isotype responses to include in analyses
#' antibodies <- c("HlyE_IgA", "HlyE_IgG")
#'
#' # Set seed to reproduce results
#' set.seed(54321)
#'
#' # Simulated incidence rate per person-year
#' lambdas = c(.05, .1, .15, .2, .3)
#'
#' # Range covered in simulations
#' lifespan <- c(0, 10);
#'
#' # Cross-sectional sample size
#' nrep <- 100
#'
#' # Biologic noise distribution
#' dlims <- rbind(
#'   "HlyE_IgA" = c(min = 0, max = 0.5),
#'   "HlyE_IgG" = c(min = 0, max = 0.5)
#' )
#'
#' sim_pop_data_multi(
#'   curve_params = dmcmc,
#'   lambdas = lambdas,
#'   n_samples = nrep,
#'   age_range = lifespan,
#'   antigen_isos = antibodies,
#'   n_mcmc_samples = 0,
#'   renew_params = TRUE,
#'   add_noise = TRUE,
#'   noise_limits = dlims,
#'   format = "long",
#'   nclus = 10)
#'
sim_pop_data_multi <- function(
    nclus = 10,
    lambdas = c(.05, .1, .15, .2, .3),
    num_cores = max(1, parallel::detectCores() - 1),
    rng_seed = 1234,
    verbose = FALSE,
    ...) {
  if (verbose) {
    message("inputs to `sim_pop_data_multi()`:")
    print(environment() %>% as.list())
  }

  if (num_cores > 1L) {
    num_cores <- num_cores %>% check_parallel_cores()

    if (verbose) {
      message("Setting up parallel processing with `num_cores` = ", num_cores, ".")
    }
  }

  doParallel::registerDoParallel(cores = num_cores)

  n_lambda <- length(lambdas)

  # trying to reproduce results using parallel
  rng <- rngtools::RNGseq(n_lambda * nclus, rng_seed)
  i <- NA
  r <- NA

  sim.df <-
    foreach::foreach(
      i = 1:length(lambdas),
      .combine = bind_rows
    ) %:%
    foreach::foreach(
      n = 1:nclus,
      r = rng[(i - 1) * nclus + 1:nclus],
      .combine = bind_rows
    ) %dopar% {
      l <- lambdas[i]
      rngtools::setRNG(r)
      sim_pop_data(
        lambda = l,
        ...
      ) %>%
        mutate(lambda.sim = l, cluster = n)
    }
  doParallel::stopImplicitCluster()
  return(sim.df)
}

#' @title Simulate multiple data sets
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `sim.cs.multi()` was renamed to [sim_pop_data_multi()]
#' to create a more consistent API.
#'
#' @keywords internal
sim.cs.multi <- function(...) { # nolint: object_name_linter
  lifecycle::deprecate_soft("1.3.1", "sim.cs.multi()", "sim_pop_data_multi()")
  sim_pop_data_multi(...)
}
