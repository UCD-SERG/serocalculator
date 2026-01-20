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
#' @param fit a `seroincidence` object from [est_seroincidence()] or a
#'   `seroincidence.by` object from [est_seroincidence_by()] that was
#'   fitted with clustering (i.e., with `cluster_var` specified)
#'
#' @return
#' * For `seroincidence` objects: A list with components `icc`, `deff`,
#'   `avg_cluster_size`, `min_cluster_size`, `max_cluster_size`, `n_clusters`,
#'   `cluster_var`, and `antigen_isos`
#' * For `seroincidence.by` objects: A data frame with one row per stratum
#'   containing the same information plus stratum identifiers
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
#'
#' # With stratified analysis
#' est_by_cluster <- est_seroincidence_by(
#'   pop_data = xs_data,
#'   strata = "catchment",
#'   sr_params = curve,
#'   noise_params = noise,
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#'   cluster_var = "cluster"
#' )
#'
#' # Calculate ICC for each stratum
#' icc_by_result <- compute_icc(est_by_cluster)
#' print(icc_by_result)
compute_icc <- function(fit) {
  UseMethod("compute_icc")
}

#' @export
compute_icc.seroincidence <- function(fit) {
  return(.compute_icc_single(fit))
}

#' @export
compute_icc.seroincidence.by <- function(fit) {
  # Process each stratum
  icc_results <- lapply(names(fit), function(stratum_name) {
    stratum_fit <- fit[[stratum_name]]
    icc_res <- .compute_icc_single(stratum_fit)
    
    # Add stratum identifier
    icc_res$Stratum <- stratum_name
    icc_res
  })
  
  # Combine into a data frame
  result_df <- do.call(rbind, lapply(icc_results, function(x) {
    data.frame(
      Stratum = x$Stratum,
      icc = x$icc,
      deff = x$deff,
      avg_cluster_size = x$avg_cluster_size,
      min_cluster_size = x$min_cluster_size,
      max_cluster_size = x$max_cluster_size,
      n_clusters = x$n_clusters,
      cluster_var = x$cluster_var,
      antigen_isos = x$antigen_isos,
      stringsAsFactors = FALSE
    )
  }))
  
  # Add strata information from attributes
  strata_info <- attr(fit, "Strata")
  if (!is.null(strata_info)) {
    result_df <- merge(
      strata_info,
      result_df,
      by = "Stratum",
      all.y = TRUE
    )
    
    # Reorder columns to put Stratum and strata variables first
    strata_cols <- setdiff(names(strata_info), "Stratum")
    other_cols <- setdiff(
      names(result_df),
      c("Stratum", strata_cols)
    )
    result_df <- result_df[, c("Stratum", strata_cols, other_cols)]
  }
  
  class(result_df) <- c("icc_seroincidence.by", "data.frame")
  return(result_df)
}

#' @export
compute_icc.default <- function(fit) {
  cli::cli_abort(c(
    "x" = paste(
      "{.arg fit} must be a {.cls seroincidence} or",
      "{.cls seroincidence.by} object from {.fun est_seroincidence}",
      "or {.fun est_seroincidence_by}."
    )
  ))
}

#' Internal function to compute ICC for a single seroincidence object
#' @noRd
#' @keywords internal
.compute_icc_single <- function(fit) {
  # Check that clustering was used
  cluster_var <- attr(fit, "cluster_var")
  if (is.null(cluster_var)) {
    cli::cli_abort(c(
      "x" = paste(
        "ICC can only be computed for models fitted with clustering.",
        "Please specify {.arg cluster_var} in {.fun est_seroincidence}."
      )
    ))
  }
  
  # Check for multiple clustering levels
  if (length(cluster_var) > 1) {
    cli::cli_abort(c(
      "x" = paste(
        "ICC calculation only allowed for one level of clustering.",
        "{.arg cluster_var} has {length(cluster_var)} levels:",
        "{cluster_var}. Please use a single cluster variable."
      )
    ))
  }

  stratum_var <- attr(fit, "stratum_var")
  antigen_isos <- attr(fit, "antigen_isos")

  # Get the combined population data
  pop_data_list <- attr(fit, "pop_data")
  pop_data_combined <- do.call(rbind, pop_data_list)

  # Get cluster information
  cluster_ids <- pop_data_combined[[cluster_var]]
  unique_clusters <- unique(cluster_ids)
  n_clusters <- length(unique_clusters)

  # Calculate cluster sizes (number of observations per cluster)
  cluster_sizes <- table(cluster_ids)
  avg_cluster_size <- mean(cluster_sizes)
  min_cluster_size <- min(cluster_sizes)
  max_cluster_size <- max(cluster_sizes)

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
    cli::cli_warn(c(
      "!" = paste(
        "Average cluster size is 1; ICC cannot be computed.",
        "Returning NA."
      )
    ))
    icc <- NA_real_
  }

  # Return results as a list
  result <- list(
    icc = icc,
    deff = deff,
    avg_cluster_size = avg_cluster_size,
    min_cluster_size = min_cluster_size,
    max_cluster_size = max_cluster_size,
    n_clusters = n_clusters,
    cluster_var = cluster_var,
    antigen_isos = paste(antigen_isos, collapse = ", ")
  )

  class(result) <- c("icc_seroincidence", "list")

  return(result)
}

#' Print method for ICC results
#'
#' @param x an object of class `icc_seroincidence` or `icc_seroincidence.by`
#' @param ... unused
#' @return invisible x
#' @export
print.icc_seroincidence <- function(x, ...) {
  cli::cli_h1("Intraclass Correlation Coefficient (ICC)")
  cli::cli_text("")
  cli::cli_text("Antigen isotypes: {.field {x$antigen_isos}}")
  cli::cli_text("Cluster variable: {.field {x$cluster_var}}")
  cli::cli_text("Number of clusters: {.val {x$n_clusters}}")
  cli::cli_text(paste(
    "Cluster size (observations per cluster): mean =",
    "{.val {round(x$avg_cluster_size, 2)}}, min =",
    "{.val {x$min_cluster_size}}, max = {.val {x$max_cluster_size}}"
  ))
  cli::cli_text("")
  cli::cli_text("Design effect (DEFF): {.val {round(x$deff, 3)}}")
  cli::cli_text("ICC: {.val {round(x$icc, 3)}}")
  cli::cli_text("")

  if (!is.na(x$icc)) {
    if (x$icc < 0) {
      cli::cli_inform(c(
        "!" = paste(
          "Negative ICC suggests no clustering effect or",
          "model misspecification."
        )
      ))
    } else if (x$icc < 0.05) {
      cli::cli_inform(c(
        "i" = "Low ICC suggests weak clustering effect."
      ))
    } else if (x$icc < 0.15) {
      cli::cli_inform(c(
        "i" = "Moderate ICC suggests notable clustering effect."
      ))
    } else {
      cli::cli_inform(c(
        "i" = "High ICC suggests strong clustering effect."
      ))
    }
  }

  invisible(x)
}

#' Print method for stratified ICC results
#'
#' @param x an object of class `icc_seroincidence.by`
#' @param ... unused
#' @return invisible x
#' @export
print.icc_seroincidence.by <- function(x, ...) {
  cli::cli_h1("Intraclass Correlation Coefficient (ICC) by Stratum")
  cli::cli_text("")
  
  # Print as a data frame
  print(tibble::as_tibble(x))
  
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
