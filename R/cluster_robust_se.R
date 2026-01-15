#' Calculate Intraclass Correlation Coefficient (ICC) for seroincidence
#'
#' @description
#' Computes the Intraclass Correlation Coefficient (ICC) for a seroincidence
#' estimate from a clustered sampling design. The ICC measures the proportion
#' of total variance that is due to between-cluster variation, indicating
#' how correlated observations within the same cluster are.
#'
#' The ICC is estimated using the design effect (DEFF):
#' \deqn{ICC = \frac{DEFF - 1}{\bar{m} - 1}}
#' where DEFF is the ratio of cluster-robust to standard variance, and
#' \eqn{\bar{m}} is the average cluster size.
#'
#' @param fit a `seroincidence` object from [est_seroincidence()] that was
#'   fitted with clustering (i.e., with `cluster_var` specified)
#'
#' @return A list with the following components:
#' * `icc`: the estimated intraclass correlation coefficient
#' * `deff`: the design effect (ratio of cluster-robust to standard variance)
#' * `avg_cluster_size`: average number of observations per cluster
#' * `n_clusters`: number of clusters
#' * `cluster_var`: name of the cluster variable used
#'
#' @export
#' @examples
#' library(dplyr)
#'
#' xs_data <- sees_pop_data_pk_100
#'
#' curve <-
#'   typhoid_curves_nostrat_100 |>
#'   filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))
#'
#' noise <- example_noise_params_pk
#'
#' # Fit model with clustering
#' est_cluster <- est_seroincidence(
#'   pop_data = xs_data,
#'   sr_params = curve,
#'   noise_params = noise,
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#'   cluster_var = "cluster"
#' )
#'
#' # Calculate ICC
#' icc_result <- compute_icc(est_cluster)
#' print(icc_result$icc)
compute_icc <- function(fit) {
  # Check that fit is a seroincidence object
  if (!inherits(fit, "seroincidence")) {
    cli::cli_abort(
      "{.arg fit} must be a {.cls seroincidence} object from
      {.fun est_seroincidence}."
    )
  }

  # Check that clustering was used
  cluster_var <- attr(fit, "cluster_var")
  if (is.null(cluster_var)) {
    cli::cli_abort(
      "ICC can only be computed for models fitted with clustering.
      Please specify {.arg cluster_var} in {.fun est_seroincidence}."
    )
  }

  stratum_var <- attr(fit, "stratum_var")

  # Get the combined population data
  pop_data_list <- attr(fit, "pop_data")
  pop_data_combined <- do.call(rbind, pop_data_list)

  # Get cluster information
  cluster_ids <- pop_data_combined[[cluster_var]]
  unique_clusters <- unique(cluster_ids)
  n_clusters <- length(unique_clusters)

  # Calculate average cluster size
  cluster_sizes <- table(cluster_ids)
  avg_cluster_size <- mean(cluster_sizes)

  # Compute standard variance (from Hessian)
  var_standard <- 1 / fit$hessian |> as.numeric()

  # Compute cluster-robust variance
  var_robust <- .compute_cluster_robust_var(
    fit = fit,
    cluster_var = cluster_var,
    stratum_var = stratum_var
  ) |> as.numeric()

  # Calculate design effect
  deff <- var_robust / var_standard

  # Calculate ICC using design effect
  # DEFF = 1 + (m̄ - 1) × ICC
  # ICC = (DEFF - 1) / (m̄ - 1)
  if (avg_cluster_size > 1) {
    icc <- (deff - 1) / (avg_cluster_size - 1)
  } else {
    cli::cli_warn(
      "Average cluster size is 1; ICC cannot be computed.
      Returning NA."
    )
    icc <- NA_real_
  }

  # Return results as a list
  result <- list(
    icc = icc,
    deff = deff,
    avg_cluster_size = avg_cluster_size,
    n_clusters = n_clusters,
    cluster_var = cluster_var
  )

  class(result) <- c("icc_seroincidence", "list")

  return(result)
}

#' Print method for ICC results
#'
#' @param x an object of class `icc_seroincidence`
#' @param ... unused
#' @return invisible x
#' @export
print.icc_seroincidence <- function(x, ...) {
  cli::cli_h1("Intraclass Correlation Coefficient (ICC)")
  cli::cli_text("")
  cli::cli_text("Cluster variable: {.field {x$cluster_var}}")
  cli::cli_text("Number of clusters: {.val {x$n_clusters}}")
  cli::cli_text("Average cluster size: {.val {round(x$avg_cluster_size, 2)}}")
  cli::cli_text("")
  cli::cli_text("Design effect (DEFF): {.val {round(x$deff, 3)}}")
  cli::cli_text("ICC: {.val {round(x$icc, 3)}}")
  cli::cli_text("")

  if (!is.na(x$icc)) {
    if (x$icc < 0) {
      cli::cli_inform(
        c(
          "!" = "Negative ICC suggests no clustering effect or
          model misspecification."
        )
      )
    } else if (x$icc < 0.05) {
      cli::cli_inform(c("i" = "Low ICC suggests weak clustering effect."))
    } else if (x$icc < 0.15) {
      cli::cli_inform(
        c("i" = "Moderate ICC suggests notable clustering effect.")
      )
    } else {
      cli::cli_inform(c("i" = "High ICC suggests strong clustering effect."))
    }
  }

  invisible(x)
}

#' Compute cluster-robust variance for seroincidence estimates
#'
#' @description
#' Computes cluster-robust (sandwich) variance estimates for seroincidence
#' parameter estimates when data come from a clustered sampling design.
#' This adjusts the standard errors to account for within-cluster correlation.
#'
#' @param fit a `seroincidence` object from [est_seroincidence()]
#' @param cluster_var name of the cluster variable in the data
#' @param stratum_var optional name of the stratum variable
#'
#' @return variance of log(lambda) accounting for clustering
#' @keywords internal
#' @noRd
.compute_cluster_robust_var <- function(
    fit,
    cluster_var,
    stratum_var = NULL) {
  # Extract stored data (already split by antigen_iso)
  pop_data_list <- attr(fit, "pop_data")
  sr_params_list <- attr(fit, "sr_params")
  noise_params_list <- attr(fit, "noise_params")
  antigen_isos <- attr(fit, "antigen_isos")

  # Get MLE estimate
  log_lambda_mle <- fit$estimate

  # Combine pop_data list back into a single data frame
  # to get cluster info
  pop_data_combined <- do.call(rbind, pop_data_list)

  # Compute score (gradient) using numerical differentiation
  # The score is the derivative of log-likelihood w.r.t. log(lambda)
  epsilon <- 1e-6

  # For each observation, compute the contribution to the score
  # We need to identify which cluster each observation belongs to
  cluster_ids <- pop_data_combined[[cluster_var]]

  # Get unique clusters
  unique_clusters <- unique(cluster_ids)
  n_clusters <- length(unique_clusters)

  # Compute cluster-level scores
  cluster_scores <- numeric(n_clusters)

  for (i in seq_along(unique_clusters)) {
    cluster_id <- unique_clusters[i]

    # Get observations in this cluster
    cluster_mask <- cluster_ids == cluster_id

    # Create temporary pop_data with only this cluster
    pop_data_cluster <- pop_data_combined[cluster_mask, , drop = FALSE]

    # Split by antigen
    pop_data_cluster_list <- split(
      pop_data_cluster,
      pop_data_cluster$antigen_iso
    )

    # Ensure all antigen_isos are represented
    # (add empty data frames if missing)
    for (ag in antigen_isos) {
      if (!ag %in% names(pop_data_cluster_list)) {
        # Create empty data frame with correct structure
        pop_data_cluster_list[[ag]] <- pop_data_list[[ag]][0, , drop = FALSE]
      }
    }

    # Compute log-likelihood for this cluster at MLE
    ll_cluster_mle <- -(.nll(
      log.lambda = log_lambda_mle,
      pop_data = pop_data_cluster_list,
      antigen_isos = antigen_isos,
      curve_params = sr_params_list,
      noise_params = noise_params_list,
      verbose = FALSE
    ))

    # Compute log-likelihood at MLE + epsilon
    ll_cluster_plus <- -(.nll(
      log.lambda = log_lambda_mle + epsilon,
      pop_data = pop_data_cluster_list,
      antigen_isos = antigen_isos,
      curve_params = sr_params_list,
      noise_params = noise_params_list,
      verbose = FALSE
    ))

    # Numerical derivative (score for this cluster)
    cluster_scores[i] <- (ll_cluster_plus - ll_cluster_mle) / epsilon
  }

  # Compute B matrix (middle of sandwich)
  # B = sum of outer products of cluster scores
  b_matrix <- sum(cluster_scores^2) # nolint: object_name_linter

  # Get Hessian (already computed by nlm)
  h_matrix <- fit$hessian # nolint: object_name_linter

  # Sandwich variance: V = H^(-1) * B * H^(-1)
  # Since we have a scalar parameter, this simplifies to:
  var_log_lambda_robust <- b_matrix / (h_matrix^2)

  return(var_log_lambda_robust)
}
