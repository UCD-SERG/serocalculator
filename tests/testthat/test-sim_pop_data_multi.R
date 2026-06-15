test_that("`sim_pop_data_multi()` works consistently", {
  skip_on_cran()
  skip_on_os("linux")
  # Load curve parameters
  dmcmc <- serocalculator::typhoid_curves_nostrat_100

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
    expect_snapshot_data(name = "pop_data_multi")
})

test_that("`sim_pop_data_multi()` handles _R_CHECK_LIMIT_CORES_ values", {
  skip_on_cran()
  dmcmc <- serocalculator::typhoid_curves_nostrat_100
  antibodies <- c("HlyE_IgA", "HlyE_IgG")
  dlims <- rbind(
    "HlyE_IgA" = c(min = 0, max = 0.5),
    "HlyE_IgG" = c(min = 0, max = 0.5)
  )
  num_cores <- 2L
  base_args <- list(
    curve_params = dmcmc,
    lambdas = 0.05,
    sample_sizes = 2,
    age_range = c(0, 1),
    antigen_isos = antibodies,
    n_mcmc_samples = 0,
    renew_params = TRUE,
    add_noise = TRUE,
    noise_limits = dlims,
    format = "long",
    nclus = 1,
    num_cores = num_cores
  )

  run_sim <- function(env_value, verbose = FALSE) {
    withr::local_seed(123)
    withr::local_envvar(c("_R_CHECK_LIMIT_CORES_" = env_value))
    do.call(sim_pop_data_multi, c(base_args, list(verbose = verbose)))
  }

  # Track the final core count passed to registerDoParallel across scenarios.
  # We mock detectCores to return a large number so the hardware-limit path
  # in check_parallel_cores() does not interfere with env-var cap testing.
  register_calls <- list()
  register_parallel <- doParallel::registerDoParallel
  testthat::local_mocked_bindings(
    detectCores = function(...) 100L,
    .package = "parallel"
  )
  testthat::local_mocked_bindings(
    registerDoParallel = function(cores) {
      register_calls <<- c(register_calls, list(cores))
      register_parallel(cores = cores)
    },
    .package = "doParallel"
  )

  sim_true <- run_sim("TRUE", verbose = TRUE)
  sim_false <- run_sim("FALSE")
  sim_numeric <- run_sim("1")
  sim_unknown <- run_sim("bogus")

  sims <- list(
    cran_cap = sim_true,
    no_limit = sim_false,
    numeric_cap = sim_numeric,
    invalid_cap = sim_unknown
  )
  expected_cols <- c("lambda.sim", "sample_size", "cluster")
  for (name in names(sims)) {
    sim_data <- sims[[name]]
    expect_s3_class(sim_data, "tbl_df")
    expect_contains(names(sim_data), expected_cols)
    expect_true(all(sim_data$sample_size == 2))
    expect_true(all(sim_data$lambda.sim == 0.05))
    expect_true(all(sim_data$cluster == 1))
  }
  # Expected final core counts after env-var cap applied by check_parallel_cores()
  expected_register_calls <- list(
    cran_cap = 2L,    # TRUE caps at 2; min(2L, 2L) = 2L
    no_limit = 2L,    # FALSE: no cap; original num_cores = 2L
    numeric_cap = 1L, # "1": cap at 1; min(2L, 1L) = 1L
    invalid_cap = 2L  # unrecognized: conservative cap at 2; min(2L, 2L) = 2L
  )
  expect_identical(register_calls[[1]], expected_register_calls$cran_cap)
  expect_identical(register_calls[[2]], expected_register_calls$no_limit)
  expect_identical(register_calls[[3]], expected_register_calls$numeric_cap)
  expect_identical(register_calls[[4]], expected_register_calls$invalid_cap)
})
