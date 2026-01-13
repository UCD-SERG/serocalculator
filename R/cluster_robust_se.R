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
.compute_cluster_robust_variance <- function(fit,
                                              cluster_var,
                                              stratum_var = NULL) {
  # Extract stored data (already split by antigen_iso)
  pop_data_list <- attr(fit, "pop_data")
  sr_params_list <- attr(fit, "sr_params")
  noise_params_list <- attr(fit, "noise_params")
  antigen_isos <- attr(fit, "antigen_isos")

  # Get MLE estimate
  log_lambda_mle <- fit$estimate

  # Combine pop_data list back into a single data frame to get cluster info
  pop_data_combined <- do.call(rbind, pop_data_list)

  # Compute score (gradient) for each observation using numerical differentiation
  # The score is the derivative of log-likelihood with respect to log(lambda)
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
    pop_data_cluster_list <- split(pop_data_cluster, pop_data_cluster$antigen_iso)

    # Ensure all antigen_isos are represented (add empty data frames if missing)
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
  B <- sum(cluster_scores^2)

  # Get Hessian (already computed by nlm)
  H <- fit$hessian

  # Sandwich variance: V = H^(-1) * B * H^(-1)
  # Since we have a scalar parameter, this simplifies to:
  var_log_lambda_robust <- B / (H^2)

  return(var_log_lambda_robust)
}
