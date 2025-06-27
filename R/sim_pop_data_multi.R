#' Simulate multiple data sets
#'
#' @param nclus  number of clusters
#' @param rng_seed starting seed for random number generator,
#' passed to [rngtools::RNGseq()]
#' @param sample_sizes sample sizes to simulate
#' @param lambdas incidence rate, in events/person*year
#' @param num_cores number of cores to use for parallel computations
#' @param verbose whether to report verbose information
#' @param ... arguments passed to [sim.cs()]
#' @inheritDotParams sim_pop_data
#' @return a [tibble::tibble()]
#' @export
#' @example inst/examples/eexm-sim_pop_data_multi.R
sim_pop_data_multi <- function(
    nclus = 10,
    sample_sizes = 100,
    lambdas = c(.05, .1, .15, .2, .3),
    num_cores = max(1, parallel::detectCores() - 1),
    rng_seed = 1234,
    verbose = FALSE,
    ...) {
  if (verbose) {
    message("inputs to `sim_pop_data_multi()`:")
    print(environment() |> as.list())
  }

  if (num_cores > 1L) {

    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")

    if (nzchar(chk) && chk == "TRUE") {
      # use 2 cores in CRAN/Travis/AppVeyor
      num_cores <- 2L
    } else {
      # use all cores in devtools::test()
      num_cores <- num_cores |> check_parallel_cores()
    }



    if (verbose) {
      cli::cli_inform("Setting up parallel processing with
                      `num_cores` = {num_cores}.")
    }
  }

  doParallel::registerDoParallel(cores = num_cores)
  n_sample_sizes <- length(sample_sizes)
  n_lambda <- length(lambdas)

  # trying to reproduce results using parallel
  rng <- rngtools::RNGseq(n_sample_sizes * n_lambda * nclus, rng_seed)

  dimnames1 <-
    list(
      "iteration" = 1:nclus,
      "lambda" = lambdas,
      "sample size" = sample_sizes
    )

  dims1 <-
    sapply(FUN = length, dimnames1)

  rng <- rng |>
    array(
      dimnames = dimnames1,
      dim = dims1
    )
  i <- NA
  j <- 1
  r <- NA

  sim_df <-
    foreach::foreach(
      .combine = bind_rows,
      j = seq_along(sample_sizes)

    ) %:%
    foreach::foreach(
      .combine = bind_rows,
      i = seq_along(lambdas)

    ) %:%
    foreach::foreach(
      .combine = bind_rows,
      n = 1:nclus,
      r = rng[1:nclus, i, j]
    ) %dopar% {
      l <- lambdas[i]
      ns <- sample_sizes[j]
      rngtools::setRNG(r)
      sim_pop_data(
        lambda = l,
        n_samples = ns,
        ...
      ) |>
        mutate(
          lambda.sim = l,
          sample_size = ns,
          cluster = n
        ) |>
        structure(r = r)
    }
  doParallel::stopImplicitCluster()
  sim_df <- sim_df |> set_biomarker_var(biomarker = "antigen_iso")
  return(sim_df)
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
#' @export
sim.cs.multi <- function(...) { # nolint: object_name_linter
  lifecycle::deprecate_soft("1.3.1", "sim.cs.multi()", "sim_pop_data_multi()")
  sim_pop_data_multi(...)
}
