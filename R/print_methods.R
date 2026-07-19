#' Print Posterior Predictive Check Results
#'
#' @param x a `posterior_predictive_check` object
#' @param ... unused
#'
#' @export
print.posterior_predictive_check <- function(x, ...) {
  cat("Posterior Predictive Check Results\n")
  cat("==================================\n\n")
  cat("Number of simulations: ", x$n_sim, "\n")
  cat("Estimated incidence rate (lambda): ", round(x$lambda_estimate, 4), " per person-year\n")
  cat("Antigen-isotypes: ", paste(x$antigen_isos, collapse = ", "), "\n")
  cat("Original data: ", nrow(x$observed), " observations\n\n")
  cat("Use autoplot() to visualize results\n")
  invisible(x)
}

#' Print Seroincidence Residuals
#'
#' @param x a `seroincidence_residuals` tibble
#' @param ... passed to [tibble::print.tbl_df()]
#'
#' @export
print.seroincidence_residuals <- function(x, ...) {
  cat("Seroincidence Residuals\n")
  cat("=======================\n\n")
  NextMethod(...)
  invisible(x)
}
