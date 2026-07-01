#' Compute cluster-robust variance for seroincidence estimates
#'
#' @description
#' Computes cluster-robust (sandwich) variance estimates for seroincidence
#' parameter estimates when data come from a clustered sampling design.
#' This adjusts the standard errors to account for within-cluster correlation.
#'
#' @details
#' For multi-way clustering the variance is assembled from all `2^k - 1`
#' non-empty subsets of the `k` cluster variables, and each subset re-runs the
#' per-cluster score computation. Cost therefore grows exponentially in the
#' number of cluster variables; this is intended for the small `k` (2-3)
#' typical of nested survey designs.
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
    stratum_var = NULL,
    small_sample = c("none", "CR1"),
    floor_to_standard = FALSE,
    debug_cluster = FALSE) {
  small_sample <- match.arg(small_sample)
  pop_data_list <- attr(fit, "pop_data")
  pop_data_combined <- do.call(rbind, pop_data_list)
  standard_var_log_lambda <- as.numeric(1 / fit$hessian)[1]

  cluster_var_combinations <- unlist(
    lapply(seq_along(cluster_var), function(n_vars) {
      utils::combn(cluster_var, n_vars, simplify = FALSE)
    }),
    recursive = FALSE
  )

  n_vars_per_subset <- vapply(cluster_var_combinations, length, integer(1))
  decomp_rows <- vector("list", length(cluster_var_combinations))

  for (i in seq_along(cluster_var_combinations)) {
    cluster_vars_subset <- cluster_var_combinations[[i]]
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
      pop_data_combined = pop_data_combined,
      small_sample = small_sample
    )
    subset_sign <- (-1)^(n_vars_per_subset[i] + 1)
    decomp_rows[[i]] <- tibble::tibble(
      subset = paste(cluster_vars_subset, collapse = " + "),
      order = n_vars_per_subset[i],
      sign = subset_sign,
      subset_variance = subset_var_log_lambda,
      signed_term = subset_sign * subset_var_log_lambda
    )
  }

  decomp_terms <- dplyr::bind_rows(decomp_rows)
  cluster_decomp <- .combine_cluster_decomp(
    decomp_terms = decomp_terms,
    standard_var = standard_var_log_lambda,
    floor_to_standard = floor_to_standard
  )

  if (debug_cluster) {
    .print_cluster_decomp(
      cluster_decomp = cluster_decomp,
      cluster_var = cluster_var,
      floor_to_standard = floor_to_standard
    )
  }

  structure(
    cluster_decomp$robust_final,
    cluster_decomp = cluster_decomp
  )
}
