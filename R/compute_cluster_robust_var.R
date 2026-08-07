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

  # Compute score (gradient) using numerical differentiation.
  # The score is the derivative of log-likelihood w.r.t. log(lambda).
  epsilon <- 1e-6

  .compute_group_scores <- function(group_ids) {
    unique_groups <- unique(group_ids)
    group_scores <- numeric(length(unique_groups))

    for (i in seq_along(unique_groups)) {
      group_id <- unique_groups[i]
      group_mask <- group_ids == group_id
      pop_data_group <- pop_data_combined[group_mask, , drop = FALSE]

      pop_data_group_list <- split(
        pop_data_group,
        pop_data_group$antigen_iso
      )

      for (ag in antigen_isos) {
        if (!ag %in% names(pop_data_group_list)) {
          pop_data_group_list[[ag]] <- pop_data_list[[ag]][0, , drop = FALSE]
        }
      }

      ll_group_plus <- -(.nll(
        log.lambda = log_lambda_mle + epsilon,
        pop_data = pop_data_group_list,
        antigen_isos = antigen_isos,
        curve_params = sr_params_list,
        noise_params = noise_params_list,
        verbose = FALSE
      ))

      ll_group_minus <- -(.nll(
        log.lambda = log_lambda_mle - epsilon,
        pop_data = pop_data_group_list,
        antigen_isos = antigen_isos,
        curve_params = sr_params_list,
        noise_params = noise_params_list,
        verbose = FALSE
      ))

      group_scores[i] <- (ll_group_plus - ll_group_minus) / (2 * epsilon)
    }

    group_scores
  }

  .compute_meat_term <- function(group_vars) {
    if (length(group_vars) == 1) {
      group_ids <- pop_data_combined[[group_vars]]
    } else {
      group_ids <- interaction(
        pop_data_combined[, group_vars, drop = FALSE],
        drop = TRUE,
        sep = "_"
      )
    }

    group_scores <- .compute_group_scores(group_ids)
    n_groups <- length(group_scores)

    if (n_groups <= 1) {
      return(0)
    }

    # Bell-McCaffrey small-sample correction for clustered meat terms.
    correction <- n_groups / (n_groups - 1)
    correction * sum(group_scores^2)
  }

  # Multi-way clustered sandwich "meat" using inclusion-exclusion.
  b_matrix <- 0 # nolint: object_name_linter
  n_cluster_dims <- length(cluster_var)

  for (subset_size in seq_len(n_cluster_dims)) {
    cluster_subsets <- utils::combn(
      cluster_var,
      subset_size,
      simplify = FALSE
    )
    sign_multiplier <- if (subset_size %% 2 == 1) 1 else -1

    for (cluster_subset in cluster_subsets) {
      b_matrix <- b_matrix +
        sign_multiplier * .compute_meat_term(cluster_subset)
    }
  }

  # Guard against tiny negative values from numerical differentiation noise.
  b_matrix <- max(0, b_matrix)

  # Get Hessian (already computed by nlm)
  h_matrix <- fit$hessian # nolint: object_name_linter

  # Sandwich variance: V = H^(-1) * B * H^(-1)
  # Since we have a scalar parameter, this simplifies to:
  var_log_lambda_robust <- b_matrix / (h_matrix^2)

  return(var_log_lambda_robust)
}
