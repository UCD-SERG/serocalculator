#' Calculate Residuals for Seroincidence Model
#'
#' @description
#' Calculates residuals for a fitted seroincidence model by computing
#' predicted antibody levels for each observation and subtracting from
#' observed levels. Useful for model diagnostics and residual analysis.
#'
#' @param object a `seroincidence` object from [est_seroincidence()]
#' @param pop_data the original population data used for fitting
#' @param standardize logical; if TRUE, return standardized residuals
#' @param verbose logical; if TRUE, print progress messages
#'
#' @details
#' Residuals are calculated as: observed - predicted
#'
#' For standardization, residuals are divided by their estimated standard error.
#' The standard error is estimated from the noise parameters in the model.
#'
#' @return
#' An object of class `"seroincidence_residuals"`, which is a tibble containing:
#' * `age`: age of individual
#' * `antigen_iso`: antigen-isotype measured
#' * `observed`: observed antibody level
#' * `predicted`: predicted antibody level from model
#' * `residual`: observed - predicted
#' * `standardized_residual`: residual / standard error
#' * `se_estimate`: estimated standard error
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
#' # Calculate residuals
#' \donttest{
#' residuals <- calculate_residuals(
#'   object = est,
#'   pop_data = xs_data,
#'   standardize = TRUE
#' )
#' }
calculate_residuals <- function(
    object,
    pop_data,
    standardize = TRUE,
    verbose = TRUE) {

  # Validate inputs
  if (!inherits(object, "seroincidence")) {
    stop("`object` must be a seroincidence object from est_seroincidence()")
  }

  if (!inherits(pop_data, "data.frame")) {
    stop("`pop_data` must be a data frame")
  }

  if (!is.logical(standardize)) {
    stop("`standardize` must be TRUE or FALSE")
  }

  # Retrieve parameters used in fitting
  sr_params <- attr(object, "sr_params")
  noise_params <- attr(object, "noise_params")
  antigen_isos <- attr(object, "antigen_isos")

  # If parameters not stored as attributes, we can't compute residuals
  if (is.null(sr_params) || is.null(noise_params)) {
    stop(
      "Cannot calculate residuals: model parameters not stored. ",
      "This typically happens when fitting an unstratified model."
    )
  }

  if (verbose) {
    cli::cli_inform("Computing predicted values for residuals...")
  }

  # Get variable names
  age_var <- get_age_var(pop_data)
  values_var <- get_values_var(pop_data)

  # Filter to antigen_isos used in fit
  data_filtered <- pop_data |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos) |>
    dplyr::filter(if_all(everything(), ~ !is.na(.x)))

  # Compute predicted values for each observation
  predicted_values <- .compute_predicted_values(
    data = data_filtered,
    sr_params = sr_params,
    noise_params = noise_params,
    age_var = age_var,
    values_var = values_var,
    antigen_isos = antigen_isos,
    verbose = verbose
  )

  # Compute residuals
  residuals_tbl <- data_filtered |>
    dplyr::mutate(
      predicted = predicted_values,
      residual = .data[[values_var]] - .data$predicted
    )

  if (standardize) {
    # Add standard error estimates
    se_estimates <- .estimate_residual_se(
      noise_params = noise_params,
      residuals_tbl = residuals_tbl,
      values_var = values_var
    )

    residuals_tbl <- residuals_tbl |>
      dplyr::mutate(
        se_estimate = se_estimates,
        standardized_residual = .data$residual / .data$se_estimate
      )
  }

  # Clean up columns for return
  result_cols <- c(
    age_var,
    "antigen_iso",
    values_var,
    "predicted",
    "residual"
  )

  if (standardize) {
    result_cols <- c(result_cols, "standardized_residual", "se_estimate")
  }

  result <- residuals_tbl |>
    dplyr::select(dplyr::all_of(result_cols)) |>
    dplyr::rename(
      age = dplyr::all_of(age_var),
      observed = dplyr::all_of(values_var)
    )

  class(result) <- c("seroincidence_residuals", class(result))

  if (verbose) {
    cli::cli_inform("Residuals computed for {nrow(result)} observations")
  }

  return(result)
}


#' @title Compute Predicted Antibody Values
#'
#' @description
#' Internal function to compute predicted antibody levels using the
#' fitted model parameters.
#'
#' @param data population data
#' @param sr_params serological response parameters
#' @param noise_params noise parameters
#' @param age_var name of age variable
#' @param values_var name of antibody values variable
#' @param antigen_isos antigens to compute predictions for
#' @param verbose logical
#'
#' @return
#' A numeric vector of predicted values
#'
#' @keywords internal
.compute_predicted_values <- function(
    data,
    sr_params,
    noise_params,
    age_var,
    values_var,
    antigen_isos,
    verbose = FALSE) {

  predicted <- numeric(nrow(data))

  # For each antigen-isotype, compute predictions
  for (antigen in antigen_isos) {
    idx <- data$antigen_iso == antigen

    # Get parameters for this antigen
    ag_params <- sr_params |>
      dplyr::filter(.data$antigen_iso == antigen)

    if (nrow(ag_params) == 0) {
      cli::cli_warn("No parameters found for {antigen}")
      next
    }

    # Average parameters across MCMC samples for single prediction
    param_avg <- ag_params |>
      dplyr::select(
        -"antigen_iso",
        -"iter"
      ) |>
      colMeans(na.rm = TRUE)

    # Create parameter matrix for ab() function
    param_matrix <- matrix(
      param_avg,
      nrow = length(param_avg),
      ncol = 1,
      dimnames = list(names(param_avg), antigen)
    )

    # Get noise limits for this antigen
    ag_noise <- noise_params |>
      dplyr::filter(.data$antigen_iso == antigen)

    if (nrow(ag_noise) == 0) {
      cli::cli_warn("No noise parameters found for {antigen}")
      next
    }

    blims <- matrix(
      c(ag_noise$y.low, ag_noise$y.high),
      nrow = 1,
      ncol = 2,
      dimnames = list(antigen, c("min", "max"))
    )

    # Compute predictions: age = time since birth
    ages <- data[idx, age_var, drop = TRUE]
    predicted_ab <- ab(
      t = ages * 365.25,
      par = param_matrix,
      blims = blims
    )

    predicted[idx] <- predicted_ab[, 1]
  }

  return(predicted)
}


#' @title Estimate Residual Standard Errors
#'
#' @description
#' Internal function to estimate standard errors for residuals based on
#' noise parameters and observed values.
#'
#' @param noise_params noise parameters
#' @param residuals_tbl residuals table
#' @param values_var name of values variable
#'
#' @return
#' A numeric vector of standard error estimates
#'
#' @keywords internal
.estimate_residual_se <- function(noise_params, residuals_tbl, values_var) {
  se_est <- numeric(nrow(residuals_tbl))

  for (i in seq_len(nrow(residuals_tbl))) {
    antigen <- residuals_tbl$antigen_iso[i]
    obs_val <- residuals_tbl[[values_var]][i]

    # Get noise parameters for this antigen
    ag_noise <- noise_params |>
      dplyr::filter(.data$antigen_iso == antigen)

    if (nrow(ag_noise) > 0) {
      # Combine biological and measurement noise in quadrature
      # se ≈ sqrt(nu^2 + eps^2 * obs_val^2)
      nu <- ag_noise$nu[1]
      eps <- ag_noise$eps[1]

      se_est[i] <- sqrt(nu^2 + (eps * obs_val)^2)
    } else {
      se_est[i] <- NA_real_
    }
  }

  return(se_est)
}
