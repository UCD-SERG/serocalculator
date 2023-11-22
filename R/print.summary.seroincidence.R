#' @title
#' Print Method for Seroincidence Summary Object
#'
#' @description
#' Custom [print()] function for "summary.seroincidence.ests" objects (constructed by [summary.seroincidence.ests()])
#'
#' @param x A "summary.seroincidence.ests" object (constructed by [summary.seroincidence.ests()])
#' @param ... Additional arguments affecting the summary produced.
#'
#' @examples
#'
#' \dontrun{
#' # estimate seroincidence
#' seroincidence <- est.incidence.by(...)
#'
#' # calculate summary statistics for the seroincidence object
#' seroincidenceSummary <- summary(seroincidence)
#'
#' # print the summary of seroincidence object to the console
#' print(seroincidenceSummary)
#'
#' # or simply type (appropriate print method will be invoked automatically)
#' seroincidenceSummary
#' }
#'
#' @export
print.summary.seroincidence.ests <- function(x, ...)
{
  cat("Seroincidence estimated given the following setup:\n")
  cat(paste("a) Antigen isotypes   :", paste(x |> attr("antigen_isos"), collapse = ", ")), "\n")
  cat(paste("b) Strata       :", paste(x |> attr("Strata"), collapse = ", ")), "\n")
  cat("\n Seroincidence estimates:\n")
  print(as_tibble(x))
  invisible(x)
}
