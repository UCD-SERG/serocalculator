#' Posterior Predictive Check for Seroincidence Model
#'
#' @description
#' Performs a posterior predictive check (PPC) by simulating datasets from
#' a fitted seroincidence model and comparing them to the observed data.
#' This helps assess whether the fitted model adequately reproduces the
#' observed data patterns.
#'
#' @param object a `seroincidence` object from [est_seroincidence()]
#' @param pop_data the original population data used for fitting
#' @param n_sim number of simulated datasets to generate
#' @param verbose logical; if TRUE, print progress messages
#' @param seed random seed for reproducibility
#'
#' @details
#' The posterior predictive check works as follows:
#' 1. Extracts the estimated incidence rate (lambda) from the fitted model
#' 2. Simulates n_sim datasets using the fitted lambda and model parameters
#' 3. Computes summary statistics for observed and simulated data
#' 4. Returns an object containing both data and summaries for visualization
#'
#' @return
#' An object of class `"posterior_predictive_check"`, which is a list containing:
#' * `observed`: the original population data
#' * `simulated`: a list of n_sim simulated datasets
#' * `observed_summary`: summary statistics for the observed data
#' * `simulated_summary`: summary statistics for each simulated dataset
#' * `lambda_estimate`: the estimated incidence rate
#' * `antigen_isos`: antigen-isotypes included in the analysis
#' * `n_sim`: number of simulations
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' xs_data <- sees_pop_data_pk_100
#'
#' curve <- typhoid_curves_nostrat_100 |>
#'   filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))
#'
#' noise <- example_noise_params_pk
#'
#' # Fit model
#' est <- est_seroincidence(
#'   pop_data = xs_data,
#'   sr_params = curve,
#'   noise_params = noise,
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA")
#' )
#'
#' # Perform posterior predictive check
#' \donttest{
#' ppc <- posterior_predictive_check(
#'   object = est,
#'   pop_data = xs_data,
#'   n_sim = 100
#' )
#' }
posterior_predictive_check <- function(
    object,
    pop_data,
    n_sim = 100,
    verbose = TRUE,
    seed = NULL) {

  # Validate inputs
  if (!inherits(object, "seroincidence")) {
    stop("`object` must be a seroincidence object from est_seroincidence()")
  }

  if (!inherits(pop_data, "data.frame")) {
    stop("`pop_data` must be a data frame")
  }

  if (!is.numeric(n_sim) || n_sim < 1 || n_sim != round(n_sim)) {
    stop("`n_sim` must be a positive integer")
  }

  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Extract attributes from fitted object
  lambda_estimate <- exp(object$estimate)
  antigen_isos <- attr(object, "antigen_isos")

  # Retrieve parameters used in fitting
  sr_params <- attr(object, "sr_params")
  noise_params <- attr(object, "noise_params")

  # If parameters not stored as attributes (ungrouped fit), we can't do PPC
  if (is.null(sr_params) || is.null(noise_params)) {
    stop(
      "Cannot perform posterior predictive check: model parameters not stored. ",
      "This typically happens when fitting an unstratified model. ",
      "Consider refitting with clustering or stratification to enable diagnostic checks."
    )
  }

  if (verbose) {
    cli::cli_inform("Starting posterior predictive check with {n_sim} simulations...")
  }

  # Simulate datasets
  simulated_data_list <- vector("list", n_sim)

  for (i in seq_len(n_sim)) {
    if (verbose && i %% 10 == 0) {
      cli::cli_inform("  Simulation {i}/{n_sim}")
    }

    tryCatch(
      {
        # Extract age range from observed data
        age_var <- get_age_var(pop_data)
        age_range <- range(pop_data[[age_var]], na.rm = TRUE)

        # Simulate data using fitted lambda
        simulated_data_list[[i]] <- sim_pop_data(
          lambda = lambda_estimate,
          n_samples = nrow(pop_data),
          age_range = age_range,
          antigen_isos = antigen_isos,
          curve_params = sr_params,
          noise_limits = .get_noise_limits(noise_params),
          n_mcmc_samples = 0,
          renew_params = TRUE,
          add_noise = TRUE,
          format = "long",
          verbose = FALSE
        )
      },
      error = function(e) {
        cli::cli_warn("Simulation {i} failed: {e$message}")
        NULL
      }
    )
  }

  # Remove any NULL entries from failed simulations
  simulated_data_list <- Filter(Negate(is.null), simulated_data_list)

  if (verbose) {
    cli::cli_inform("Posterior predictive check complete. {length(simulated_data_list)} simulations succeeded.")
  }

  # Compute summary statistics
  observed_summary <- .compute_ppc_summary(pop_data, antigen_isos)
  simulated_summary <- lapply(
    simulated_data_list,
    .compute_ppc_summary,
    antigen_isos = antigen_isos
  )

  # Create result object
  result <- list(
    observed = pop_data,
    simulated = simulated_data_list,
    observed_summary = observed_summary,
    simulated_summary = simulated_summary,
    lambda_estimate = lambda_estimate,
    antigen_isos = antigen_isos,
    n_sim = length(simulated_data_list),
    fitted_model = object
  )

  class(result) <- c("posterior_predictive_check", "list")

  return(result)
}


#' @title Compute Summary Statistics for PPC
#'
#' @description
#' Internal helper function to compute summary statistics for a dataset
#' used in posterior predictive checks.
#'
#' @param data a population data tibble/data.frame
#' @param antigen_isos character vector of antigen isotypes
#'
#' @return
#' A tibble with summary statistics for each antigen by age group
#'
#' @keywords internal
.compute_ppc_summary <- function(data, antigen_isos) {
  age_var <- get_age_var(data)
  values_var <- get_values_var(data)

  # Create age groups for summarization
  data_aug <- data |>
    dplyr::mutate(
      age_group = cut(.data[[age_var]],
        breaks = seq(
          floor(min(.data[[age_var]], na.rm = TRUE)),
          ceiling(max(.data[[age_var]], na.rm = TRUE)),
          by = 2
        ),
        include.lowest = TRUE
      )
    ) |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos)

  # Compute summary statistics
  summary_stats <- data_aug |>
    dplyr::group_by(.data$antigen_iso, .data$age_group) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_level = mean(.data[[values_var]], na.rm = TRUE),
      median_level = stats::median(.data[[values_var]], na.rm = TRUE),
      sd_level = stats::sd(.data[[values_var]], na.rm = TRUE),
      min_level = min(.data[[values_var]], na.rm = TRUE),
      max_level = max(.data[[values_var]], na.rm = TRUE),
      q25 = stats::quantile(.data[[values_var]], 0.25, na.rm = TRUE),
      q75 = stats::quantile(.data[[values_var]], 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  return(summary_stats)
}


#' @title Extract Noise Limits from Noise Parameters
#'
#' @description
#' Internal helper to extract noise limits matrix from noise_params tibble
#'
#' @param noise_params a noise_params tibble
#'
#' @return
#' A matrix of noise limits suitable for sim_pop_data
#'
#' @keywords internal
.get_noise_limits <- function(noise_params) {
  if (!is.data.frame(noise_params)) {
    return(noise_params)
  }

  # Create matrix with antigen_iso as rownames
  antigen_isos <- noise_params$antigen_iso
  noise_mat <- matrix(
    c(noise_params$y.low, noise_params$y.high),
    nrow = length(antigen_isos),
    ncol = 2,
    dimnames = list(antigen_isos, c("min", "max"))
  )

  return(noise_mat)
}
