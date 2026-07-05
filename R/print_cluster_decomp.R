#' Print a cluster-robust variance decomposition
#'
#' @description
#' Emits the concise `V_*` diagnostics for a multi-way cluster-robust variance
#' decomposition when `debug_cluster = TRUE`. Separated from the computation so
#' that [.compute_cluster_robust_var()] stays focused on the variance itself.
#'
#' @param cluster_decomp the list returned by [.combine_cluster_decomp()]
#' @param cluster_var name(s) of the cluster variable(s), used only to label a
#'   two-way term as `V_intersection`
#' @param floor_to_standard whether the model-based variance floor was in effect
#'
#' @return `NULL`, invisibly; called for its printed side effect
#' @keywords internal
#' @noRd
.print_cluster_decomp <- function(
    cluster_decomp,
    cluster_var,
    floor_to_standard) {
  decomp_terms <- cluster_decomp$terms
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

  cli::cli_inform("V_raw = {signif(cluster_decomp$robust_raw, 6)}")
  cli::cli_inform("V_final = {signif(cluster_decomp$robust_final, 6)}")

  if (isTRUE(floor_to_standard)) {
    floor_message <- glue::glue(
      "Variance floor relative to standard variance ",
      "{signif(cluster_decomp$standard_var, 6)}: ",
      "{if (cluster_decomp$floor_applied) 'applied' else 'not applied'}"
    )
    cli::cli_inform(floor_message)
  }

  invisible(NULL)
}
