#' Helper function to convert single seroincidence ICC result to data frame row
#' @noRd
#' @keywords internal
.icc_single_to_df_row <- function(stratum_name, stratum_fit) {
  icc_res <- .compute_icc_single(stratum_fit)

  # Add stratum identifier
  icc_res$Stratum <- stratum_name

  # Convert to data frame row
  data.frame(
    Stratum = icc_res$Stratum,
    icc = icc_res$icc,
    deff = icc_res$deff,
    avg_cluster_size = icc_res$avg_cluster_size,
    min_cluster_size = icc_res$min_cluster_size,
    max_cluster_size = icc_res$max_cluster_size,
    n_clusters = icc_res$n_clusters,
    cluster_var = icc_res$cluster_var,
    antigen_isos = icc_res$antigen_isos,
    stringsAsFactors = FALSE
  )
}
