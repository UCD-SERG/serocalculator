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
  pop_data_list <- attr(fit, "pop_data")
  pop_data_combined <- do.call(rbind, pop_data_list)
  standard_var_log_lambda <- 1 / fit$hessian |> as.numeric()

  subset_cluster_vars <- unlist(
    lapply(seq_along(cluster_var), function(n_vars) {
      utils::combn(cluster_var, n_vars, simplify = FALSE)
    }),
    recursive = FALSE
  )

  cluster_var_terms <- vapply(subset_cluster_vars, length, integer(1))
  robust_var_log_lambda <- 0

  for (i in seq_along(subset_cluster_vars)) {
    cluster_vars_subset <- subset_cluster_vars[[i]]
    if (length(cluster_vars_subset) == 1) {
      cluster_ids <- pop_data_combined[[cluster_vars_subset]]
    } else {
      cluster_ids <- interaction(
        pop_data_combined[, cluster_vars_subset, drop = FALSE],
        drop = TRUE,
        sep = "_"
      )
    }

    subset_var_log_lambda <- .compute_cluster_var_oneway(
      fit = fit,
      cluster_ids = cluster_ids,
      pop_data_combined = pop_data_combined
    )
    robust_var_log_lambda <- robust_var_log_lambda +
      (-1)^(cluster_var_terms[[i]] + 1) * subset_var_log_lambda
  }

  # Clustering should not reduce uncertainty relative to the model-based
  # standard error when the only adjustment is for within-cluster correlation.
  robust_var_log_lambda <- max(
    standard_var_log_lambda,
    robust_var_log_lambda
  )

  return(robust_var_log_lambda)
}
