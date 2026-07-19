#' Goodness of Fit Summary for Seroincidence Model
#'
#' @description
#' Computes a comprehensive set of goodness of fit metrics for a fitted
#' seroincidence model, including log-likelihood, AIC, BIC, pseudo-R²,
#' and residual diagnostics.
#'
#' @param object a `seroincidence` object from [est_seroincidence()]
#' @param pop_data the original population data used for fitting
#' @param verbose logical; if TRUE, print detailed output
#'
#' @details
#' Goodness of fit metrics computed:
#' * `log_likelihood`: Log-likelihood of the data at the MLE
#' * `aic`: Akaike Information Criterion (AIC = 2k - 2*LL)
#' * `bic`: Bayesian Information Criterion (BIC = k*log(n) - 2*LL)
#' * `n_obs`: Number of observations
#' * `n_params`: Number of parameters (1 for incidence rate)
#' * `pseudo_r2`: Pseudo-R² based on comparison to null model
#' * `mean_residual`: Mean of residuals (should be near 0)
#' * `sd_residual`: Standard deviation of residuals
#' * `residual_skewness`: Skewness of residuals
#' * `residual_kurtosis`: Excess kurtosis of residuals
#' * `shapiro_pvalue`: p-value from Shapiro-Wilk test of residual normality
#'
#' The pseudo-R² is computed as: 1 - (LogLik_model / LogLik_null)
#' where the null model assumes all observations are independent with equal weight.
#'
#' @return
#' A tibble with a single row containing all goodness of fit metrics
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
#' # Fit model (works with both grouped and ungrouped fits)
#' est <- est_seroincidence(
#'   pop_data = xs_data,
#'   sr_params = curve,
#'   noise_params = noise,
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA")
#' )
#'
#' # Get goodness of fit summary
#' gof <- gof_summary(
#'   object = est,
#'   pop_data = xs_data,
#'   verbose = FALSE
#' )
gof_summary <- function(object, pop_data, verbose = TRUE) {

  # Validate inputs
  if (!inherits(object, "seroincidence")) {
    stop("`object` must be a seroincidence object from est_seroincidence()")
  }

  if (!inherits(pop_data, "data.frame")) {
    stop("`pop_data` must be a data frame")
  }

  if (verbose) {
    cli::cli_inform("Computing goodness of fit summary...")
  }

  # Extract key quantities
  log_lik <- -object$minimum
  n_obs <- nrow(pop_data)
  n_params <- 1

  # Calculate AIC and BIC
  aic <- 2 * n_params - 2 * log_lik
  bic <- n_params * log(n_obs) - 2 * log_lik

  # Compute residuals for additional diagnostics
  residuals_tbl <- tryCatch(
    {
      calculate_residuals(object, pop_data, standardize = TRUE, verbose = FALSE)
    },
    error = function(e) {
      if (verbose) {
        cli::cli_inform(c(
          "i" = "Residual diagnostics not available: {e$message}"
        ))
      }
      NULL
    }
  )

  # Compute residual statistics if available
  if (!is.null(residuals_tbl)) {
    residuals_vec <- residuals_tbl$residual
    std_residuals <- residuals_tbl$standardized_residual

    mean_resid <- mean(residuals_vec, na.rm = TRUE)
    sd_resid <- stats::sd(residuals_vec, na.rm = TRUE)
    skewness_resid <- .compute_skewness(residuals_vec)
    kurtosis_resid <- .compute_kurtosis(residuals_vec)

    # Shapiro-Wilk test (only if sample size is reasonable)
    if (length(std_residuals) > 3 && length(std_residuals) <= 5000) {
      sw_test <- stats::shapiro.test(std_residuals)
      shapiro_pvalue <- sw_test$p.value
    } else {
      shapiro_pvalue <- NA_real_
    }
  } else {
    mean_resid <- NA_real_
    sd_resid <- NA_real_
    skewness_resid <- NA_real_
    kurtosis_resid <- NA_real_
    shapiro_pvalue <- NA_real_
  }

  # Compute pseudo-R²
  pseudo_r2 <- .compute_pseudo_r2(object, pop_data)

  # Create result tibble
  result <- tibble::tibble(
    log_likelihood = log_lik,
    aic = aic,
    bic = bic,
    n_observations = n_obs,
    n_parameters = n_params,
    pseudo_r2 = pseudo_r2,
    mean_residual = mean_resid,
    sd_residual = sd_resid,
    residual_skewness = skewness_resid,
    residual_kurtosis = kurtosis_resid,
    shapiro_wilk_pvalue = shapiro_pvalue
  )

  if (verbose) {
    cli::cli_inform("Goodness of fit summary computed.")
  }

  return(result)
}


#' @title Compute Skewness
#'
#' @description
#' Internal function to compute skewness of a vector
#'
#' @param x numeric vector
#'
#' @return
#' Skewness value
#'
#' @keywords internal
.compute_skewness <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3) return(NA_real_)

  n <- length(x)
  m <- mean(x)
  s <- stats::sd(x)

  if (s == 0) return(NA_real_)

  return((sum((x - m)^3) / n) / (s^3))
}


#' @title Compute Excess Kurtosis
#'
#' @description
#' Internal function to compute excess kurtosis of a vector
#'
#' @param x numeric vector
#'
#' @return
#' Excess kurtosis value
#'
#' @keywords internal
.compute_kurtosis <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 4) return(NA_real_)

  n <- length(x)
  m <- mean(x)
  s <- stats::sd(x)

  if (s == 0) return(NA_real_)

  return((sum((x - m)^4) / n) / (s^4) - 3)
}


#' @title Compute Pseudo-R²
#'
#' @description
#' Internal function to compute pseudo-R² for the model
#'
#' @param object a seroincidence object
#' @param pop_data population data
#'
#' @return
#' Pseudo-R² value
#'
#' @keywords internal
.compute_pseudo_r2 <- function(object, pop_data) {
  # Get the log-likelihood of fitted model
  ll_model <- -object$minimum

  # Get parameters from attributes
  sr_params <- attr(object, "sr_params")
  noise_params <- attr(object, "noise_params")
  antigen_isos <- attr(object, "antigen_isos")

  if (is.null(sr_params) || is.null(noise_params)) {
    return(NA_real_)
  }

  # Compute null model log-likelihood
  # (using mean lambda = sum(infections) / sum(person-years))
  age_var <- get_age_var(pop_data)

  # Rough approximation: null model assumes lambda = 0.01 (very low incidence)
  ll_null <- tryCatch(
    {
      log_likelihood(
        lambda = 0.01,
        pop_data = pop_data,
        curve_params = sr_params,
        noise_params = noise_params,
        antigen_isos = antigen_isos,
        verbose = FALSE
      )
    },
    error = function(e) NA_real_
  )

  if (is.na(ll_null)) {
    return(NA_real_)
  }

  # Pseudo-R² = 1 - (LogLik_model / LogLik_null)
  # Both are negative, so this gives a value between 0 and 1 if model is better
  pseudo_r2 <- 1 - (ll_model / ll_null)

  # Constrain to reasonable bounds
  pseudo_r2 <- pmax(0, pmin(1, pseudo_r2))

  return(pseudo_r2)
}
