#' Compute cluster-robust variance for seroincidence estimates
#'
#' @description
#' Computes cluster-robust (sandwich) variance estimates for seroincidence
#' parameter estimates when data come from a clustered sampling design.
#' This adjusts the standard errors to account for within-cluster correlation.
#'
#' @param fit a `seroincidence` object from [est_seroincidence()]
#' @param cluster_var name(s) of the cluster variable(s) in the data.
#'   Can be a single variable or vector of variables for multi-level clustering.
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

  # Handle multiple clustering levels by creating composite cluster ID
  if (length(cluster_var) == 1) {
    cluster_ids <- pop_data_combined[[cluster_var]]
  } else {
    # Create composite cluster ID from multiple variables
    cluster_ids <- interaction(
      pop_data_combined[, cluster_var, drop = FALSE],
      drop = TRUE,
      sep = "_"
    )
  }

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
