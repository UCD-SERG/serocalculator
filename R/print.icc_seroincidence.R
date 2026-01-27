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
