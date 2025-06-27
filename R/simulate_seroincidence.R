#' Simulate seroincidence
#'
#' @param nrep
#' @param n_sim
#'
#' @returns
#' @export
#' @keywords internal
#'
#' @examples
simulate_seroincidence <- function(nrep, n_sim) {
  # Parameters
  dmcmc <- test_sim  # Curve parameters
  antibodies <-c("HlyE_IgA", "HlyE_IgG")
  lambda <- 0.01  # Simulated incidence rate per person-year
  lifespan <- c(0, 10)  # Age range

  # biologic noise distribution
  dlims <- rbind(
    "HlyE_IgA" = c(min = 0, max = 0.5),
    "HlyE_IgG" = c(min = 0, max = 0.5)
  )

  # Noise parameters
  cond <- tibble(
    antigen_iso = c("HlyE_IgG", "HlyE_IgA"),
    nu = c(0.5, 0.5),  # Biologic noise (nu)
    eps = c(0, 0),     # Measurement noise (eps)
    y.low = c(1, 1),   # Low cutoff (llod)
    y.high = c(5e6, 5e6)  # High cutoff (y.high)
  )

  # Perform simulations in parallel
  results <- future_map(1:n_sim, function(i) {
    # Generate cross-sectional data
    csdata <- sim.cs(
      curve_params = dmcmc,
      lambda = lambda,
      n.smpl = nrep,
      age.rng = lifespan,
      antigen_isos = antibodies,
      n.mc = 0,
      renew.params = TRUE,  # Use different parameters for each simulation
      add.noise = TRUE,
      noise_limits = dlims,
      format = "long"
    )

    # Estimate seroincidence
    est <- est.incidence(
      pop_data = csdata,
      curve_params = dmcmc,
      noise_params = cond,
      lambda_start = 0.005,
      build_graph = TRUE,
      verbose = FALSE,
      print_graph = FALSE,
      antigen_isos = antibodies
    )

    # Return results for this simulation
    list(
      csdata = csdata,
      est1 = est
    )
  }, .options = furrr_options(seed = TRUE))

  return(results)
}
