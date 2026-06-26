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
  robust_raw <- sum(decomp_terms$signed_term)
  floor_applied <- isTRUE(floor_to_standard) &&
    robust_raw < standard_var_log_lambda
  robust_final <- if (isTRUE(floor_to_standard)) {
    max(standard_var_log_lambda, robust_raw)
  } else {
    robust_raw
  }

  if (debug_cluster) {
    cli::cli_inform("Cluster-robust variance decomposition:")

    for (i in seq_len(nrow(decomp_terms))) {
      term_label <- if (
        length(cluster_var) == 2 &&
          decomp_terms$order[i] == 2
      ) {
        "V_intersection"
      } else {
        paste0(
          "V_",
          gsub(" \\+ ", "_", decomp_terms$subset[i])
        )
      }
      term_message <- glue::glue(
        "{term_label} ({decomp_terms$subset[i]}) = ",
        "{signif(decomp_terms$subset_variance[i], 6)}"
      )

      cli::cli_inform(term_message)
    }

    cli::cli_inform("V_raw = {signif(robust_raw, 6)}")
    cli::cli_inform("V_final = {signif(robust_final, 6)}")

    if (isTRUE(floor_to_standard)) {
      floor_message <- glue::glue(
        "Variance floor relative to standard variance ",
        "{signif(standard_var_log_lambda, 6)}: ",
        "{if (floor_applied) 'applied' else 'not applied'}"
      )
      cli::cli_inform(floor_message)
    }
  }

  structure(
    robust_final,
    cluster_decomp = list(
      standard_var = standard_var_log_lambda,
      robust_raw = robust_raw,
      robust_final = robust_final,
      terms = decomp_terms,
      floor_applied = floor_applied
    )
  )
}
