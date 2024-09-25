#' @title
#' Print Method for `seroincidence` Object
#'
#' @description
#' Custom [print()] function to show output of the seroincidence calculator [est.incidence()].
#'
#' @param x A list containing output of function [est.incidence.by()].
#' @param ... Additional arguments affecting the summary produced.
#'
#' @examples
#' \dontrun{
#' # Estimate seroincidence
#' seroincidence <- est.incidence.by(...)
#'
#' # Print the seroincidence object to the console
#' print(seroincidence)
#'
#' # Or simply type (appropriate print method will be invoked automatically)
#' seroincidence
#' }
#'
#' @export
print.seroincidence <- function(x, ...) {
  cat("`seroincidence` object estimated given the following setup:\n")
  cat(paste("a) `antigen_isos`: ", paste(attr(x, "antigen_isos"), collapse = ", ")), "\n")
  cat(paste("b) `lambda_start`: ", attr(x, "lambda_start"), "\n"))
  cat("Call the `summary()` function to obtain output results.\n")
  cat("Call the `autoplot()` function to graph the log-likelihood curve.\n")
  invisible(x)
}
