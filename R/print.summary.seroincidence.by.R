#' @title
#' Print Method for Seroincidence Summary Object
#'
#' @description
#' Custom [print()] function for "summary.seroincidence.by" objects (constructed by [summary.seroincidence.by()])
#'
#' @param x A "summary.seroincidence.by" object (constructed by [summary.seroincidence.by()])
#' @param ... Additional arguments affecting the summary produced.
#'
#' @examples
#' \dontrun{
#' # Estimate seroincidence
#' seroincidence <- est.incidence.by(...)
#'
#' # Calculate summary statistics for the seroincidence object
#' seroincidenceSummary <- summary(seroincidence)
#'
#' # Print the summary of seroincidence object to the console
#' print(seroincidenceSummary)
#'
#' # Or simply type (appropriate print method will be invoked automatically)
#' seroincidenceSummary
#' }
#'
#' @export
print.summary.seroincidence.by <- function(x, ...) {
  cat("Seroincidence estimated given the following setup:\n")
  cat(paste("a) Antigen isotypes   :", paste(x %>% attr("antigen_isos"), collapse = ", ")), "\n")
  cat(paste("b) Strata       :", paste(x %>% attr("Strata"), collapse = ", ")), "\n")
  cat("\n Seroincidence estimates:\n")
  print(as_tibble(x))
  invisible(x)
}
