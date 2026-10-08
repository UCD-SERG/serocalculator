#' Compute one-way cluster-robust variance for seroincidence estimates
#'
#' @param fit a `seroincidence` object from [est_seroincidence()]
#' @param cluster_ids cluster identifier for each row in `pop_data_combined`
#' @param pop_data_combined combined population data across antigen isotypes
#' @param small_sample small-sample correction to apply. `"CR1"` multiplies the
#'   variance by `G / (G - 1)`, where `G` is the number of clusters; `"none"`
#'   applies no correction.
#'
#' @return one-way cluster-robust variance of log(lambda)
#' @keywords internal
#' @noRd
.compute_cluster_var_oneway <- function(
  fit,
  cluster_ids,
  pop_data_combined,
  small_sample = c("CR1", "none")
) {
  small_sample <- match.arg(small_sample)
  pop_data_list <- attr(fit, "pop_data")
  sr_params_list <- attr(fit, "sr_params")
  noise_params_list <- attr(fit, "noise_params")
  antigen_isos <- attr(fit, "antigen_isos")
  method <- attr(fit, "method")
  if (is.null(method)) method <- "composite"
  id_var <- attr(fit, "id_var")
  # Recomputing the score under a different quadrature from the one the
  # fit was optimized under would build the sandwich's middle matrix from
  # a different objective than its Hessian. NULL means the fit used the
  # default, so omitting the argument reproduces it.
  n_t_steps <- attr(fit, "n_t_steps")
  log_lambda_mle <- fit$estimate
  epsilon <- 1e-6

  unique_clusters <- unique(cluster_ids)
  n_clusters <- length(unique_clusters)
  cluster_scores <- numeric(n_clusters)

  for (i in seq_along(unique_clusters)) {
    cluster_id <- unique_clusters[i]
    cluster_mask <- cluster_ids == cluster_id
    pop_data_cluster <- pop_data_combined[cluster_mask, , drop = FALSE]
    pop_data_cluster_list <- split(
      pop_data_cluster,
      pop_data_cluster$antigen_iso
    )

    for (ag in antigen_isos) {
      if (!ag %in% names(pop_data_cluster_list)) {
        pop_data_cluster_list[[ag]] <- pop_data_list[[ag]][0, , drop = FALSE]
      }
    }

    nll_args <- list(
      pop_data = pop_data_cluster_list,
      antigen_isos = antigen_isos,
      curve_params = sr_params_list,
      noise_params = noise_params_list,
      verbose = FALSE,
      method = method,
      id_var = id_var
    )
    if (!is.null(n_t_steps)) {
      nll_args$n_t_steps <- n_t_steps
    }

    ll_cluster_mle <- -(do.call(
      .nll,
      c(list(log.lambda = log_lambda_mle), nll_args)
    ))
    ll_cluster_plus <- -(do.call(
      .nll,
      c(list(log.lambda = log_lambda_mle + epsilon), nll_args)
    ))

    cluster_scores[i] <- (ll_cluster_plus - ll_cluster_mle) / epsilon
  }

  score_variance <- sum(cluster_scores^2)
  hessian <- as.numeric(fit$hessian)[1]

  if (!is.finite(hessian) || hessian <= 0) {
    cli::cli_warn(c(
      "!" = "The Hessian for a one-way cluster-robust variance term was
        non-finite or non-positive ({signif(hessian, 6)}); returning a
        missing value for this term.",
      "i" = "This usually means the numerical derivatives did not converge,
        and the affected cluster-robust standard error will be {.val {NA}}."
    ))
    return(NA_real_)
  }

  var_log_lambda <- score_variance / (hessian^2)

  if (small_sample == "CR1" && n_clusters > 1) {
    var_log_lambda <- var_log_lambda * (n_clusters / (n_clusters - 1))
  }

  var_log_lambda
}
