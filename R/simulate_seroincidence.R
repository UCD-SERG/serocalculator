#' Simulate seroincidence
#'
#' @param nrep number of samples per simulated dataset
#' @param n_sim number of simulations to run
#' @param renew.params whether to sample new parameters for each infection
#' in a simulated individual's longitudinal data
#' @param sr_params Curve parameters
#' @param antibodies which antibodies to simulate
#' @param lambda Simulated incidence rate per person-year
#' @param lifespan range of ages to simulate
#' @param dlims biologic noise distribution
#' @param cond Noise parameters
#'
#' @returns a [list] of simulation results
#' @keywords internal
#'
#' @examples
#' simulate_seroincidence(sr_params = typhoid_curves_nostrat_100)
simulate_seroincidence <- function(
    nrep = 100,
    n_sim = 10,
    sr_params,
    renew.params = TRUE,
    antibodies = c("HlyE_IgA", "HlyE_IgG"),
    lambda = 0.01,
    lifespan = c(0, 10),  # Age range

    #
    dlims = rbind(
      "HlyE_IgA" = c(min = 0, max = 0.5),
      "HlyE_IgG" = c(min = 0, max = 0.5)
    ),

    #
    cond = tibble(
      antigen_iso = c("HlyE_IgG", "HlyE_IgA"),
      nu = c(0.5, 0.5),  # Biologic noise (nu)
      eps = c(0, 0),     # Measurement noise (eps)
      y.low = c(1, 1),   # Low cutoff (llod)
      y.high = c(5e6, 5e6)  # High cutoff (y.high)
    )
) {
  # Parameters
  dmcmc <- sr_params  #


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
      renew.params = renew.params,  # Use different parameters for each simulation
      add.noise = TRUE,
      noise_limits = dlims,
      format = "long"
    )

    # Estimate seroincidence
    est <- est_seroincidence(
      pop_data = csdata,
      sr_params = dmcmc,
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

  results <-
    results |>
    structure(
      lambda_true = lambda,
      sample_size = nrep)

  return(results)
}
