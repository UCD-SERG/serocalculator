#' @title
#' Print Method for Seroincidence Object
#'
#' @description
#' Custom [print()] function to show output of the seroincidence calculator [est.incidence.by()].
#'
#' @param x A list containing output of function [est.incidence.by()].
#' @param ... Additional arguments affecting the summary produced.
#'
#' @examples
#'
#' \dontrun{
#' # estimate seroincidence
#' seroincidence <- est.incidence.by(...)
#'
#' # print the seroincidence object to the console
#' print(seroincidence)
#'
#' # or simply type (appropriate print method will be invoked automatically)
#' seroincidence
#' }
#'
#' @export
print.seroincidence.ests <- function(x, ...)
{
  cat("Seroincidence object estimated given the following setup:\n")
  cat(paste("a) Antibodies   :", paste(attr(x, "Antibodies"), collapse = ", ")), "\n")
  cat(paste("b) Strata       :", paste(attr(x, "Strata") |> names(), collapse = ", ")), "\n")

    cat("\n")
  cat("This object is a list containing the following items:\n")
  cat("Fits         - List of outputs of `nlm()` function per stratum.\n")
  cat("Antibodies   - Input parameter antibodies of function \"est.incidence.by\".\n")
  cat("Strata       - Input parameter strata of function `est.incidence.by()`\n")
  cat("\n")
  cat("Call summary function to obtain output results.\n")
  invisible(x)
}
