#' Inform the caller that a multi-biomarker fit assumes independence
#' across biomarkers
#'
#' @description
#' `log_likelihood()` combines biomarkers by summing their marginal
#' log-likelihoods (a composite/independence likelihood) rather than
#' integrating a shared latent infection time --- see issue #637.
#' The naive standard error therefore understates the true variance
#' whenever biomarker readings from the same person are correlated,
#' as is expected when they share an infection history --- see #645.
#' This emits an informational message pointing the caller at
#' `cluster_var` as the fix, unless they've already supplied one, or
#' asked for the joint likelihood (`method = "joint"`), which has no
#' independence assumption to correct for.
#' [est_seroincidence_by()] calls this once per stratum, so a
#' stratified verbose fit repeats the message.
#'
#' @param pop_data the original `pop_data` object passed to
#' [est_seroincidence()], before filtering removes its `id_var`
#' attribute
#' @param antigen_isos the antigen isotypes being combined
#' @param cluster_var the `cluster_var` argument as passed by the
#' caller (`NULL` if not supplied)
#' @param verbose whether to emit the message
#' @param method the `method` argument as passed by the caller
#'
#' @return `invisible(NULL)`
#' @keywords internal
.warn_biomarker_independence <- function(
  pop_data,
  antigen_isos,
  cluster_var,
  verbose,
  method = "composite"
) {
  skip <- !verbose ||
    length(antigen_isos) < 2 ||
    !is.null(cluster_var) ||
    method == "joint"
  if (skip) {
    return(invisible(NULL))
  }

  cli::cli_inform(c(
    "i" = "Combining {length(antigen_isos)} biomarkers by summing
    their marginal log-likelihoods assumes independence across
    biomarkers.",
    "i" = "The reported standard error will be too narrow if
    biomarker readings from the same person are correlated (e.g.,
    because they share an infection history)."
  ))

  id_var <- suppressWarnings(
    tryCatch(ids_varname(pop_data), error = function(e) NULL)
  )

  if (!is.null(id_var)) {
    cli::cli_inform(c(
      "i" = "Pass {.code cluster_var = \"{id_var}\"} for a
      cluster-robust standard error that accounts for this.",
      "i" = "If the data also has a clustered sampling design (e.g.,
      households, schools), clustering on {.field {id_var}} alone
      does not add that correction when subjects are nested within
      it --- see issue #543."
    ))
  }

  invisible(NULL)
}
