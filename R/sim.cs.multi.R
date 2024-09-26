#' Simulate multiple data sets
#'
#' @param nclus  number of clusters
#' @param rng_seed starting seed for random number generator, passed to [rngtools::RNGseq()]
#' @param lambdas #incidence rate, in events/person*year
#' @param num_cores number of cores to use for parallel computations
#' @param verbose whether to report verbose information
#' @param ... arguments passed to [simulate_pop_data()]
#' @inheritParams simulate_pop_data
#' @inheritDotParams simulate_pop_data
#' @return a [tibble::tibble()]
#' @export
#'

simulate_pop_data.multi <- function(
    nclus = 10,
    lambdas = c(.05, .1, .15, .2, .3),
    num_cores = max(1, parallel::detectCores() - 1),
    rng_seed = 1234,
    renew.params = TRUE,
    add.noise = TRUE,
    verbose = FALSE,
    ...) {
  if (verbose) {
    message("inputs to `simulate_pop_data.multi()`:")
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
      simulate_pop_data(
        lambda = l,
        renew.params = renew.params,
        add.noise = add.noise,
        ...
      ) %>%
        mutate(lambda.sim = l, cluster = n)
    }
  doParallel::stopImplicitCluster()
  return(sim.df)
}
