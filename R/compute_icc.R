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
#' @example inst/examples/exm-compute_icc.R
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
    .icc_single_to_df_row(stratum_name, stratum_fit)
  })

  # Combine into a data frame
  result_df <- do.call(rbind, icc_results)

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
