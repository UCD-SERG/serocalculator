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
