#' @title
#' Print Method for Seroincidence Summary Object
#'
#' @description
#' Custom [print()] function to show output of the seroincidence summary [summary.seroincidence()].
#'
#' @param x A list containing output of function [summary.seroincidence()].
#' @param ... Additional arguments affecting the summary produced.
#'
#' @examples
#'
#' \dontrun{
#' # estimate seroincidence
#' seroincidence <- estimateSeroincidence(...)
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
print.summary.seroincidence <- function(x, ...)
{
  cat("Seroincidence estimated given the following setup:\n")
  cat(paste("a) Antibodies   :", paste(x |> attr("Antibodies"), collapse = ", ")), "\n")
  cat(paste("b) Strata       :", paste(x |> attr("Strata"), collapse = ", ")), "\n")
  cat("\n Seroincidence estimates:\n")
  print(as_tibble(x))
}
