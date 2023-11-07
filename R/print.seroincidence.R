#' @title
#' Print Method for Seroincidence Object
#'
#' @description
#' Custom [print()] function to show output of the seroincidence calculator [estimateSeroincidence()].
#'
#' @param x A list containing output of function [estimateSeroincidence()].
#' @param ... Additional arguments affecting the summary produced.
#'
#' @examples
#'
#' \dontrun{
#' # estimate seroincidence
#' seroincidence <- estimateSeroincidence(...)
#'
#' # print the seroincidence object to the console
#' print(seroincidence)
#'
#' # or simply type (appropriate print method will be invoked automatically)
#' seroincidence
#' }
#'
#' @export
print.seroincidenceList = print.seroincidence <- function(x, ...)
{
  cat("Seroincidence object estimated given the following setup:\n")
  cat(paste("a) Antibodies   :", paste(attr(x, "Antibodies"), collapse = ", ")), "\n")
  cat(paste("b) Strata       :", paste(attr(x, "Strata"), collapse = ", ")), "\n")

    cat("\n")
  cat("This object is a list containing the following items:\n")
  cat("Fits         - List of outputs of `nlm()` function per stratum.\n")
  cat("Antibodies   - Input parameter antibodies of function \"estimateSeroincidence\".\n")
  cat("Strata       - Input parameter strata of function \"estimateSeroincidence\".\n")
  cat("\n")
  cat("Call summary function to obtain output results.\n")
}
