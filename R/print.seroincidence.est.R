#' @title
#' Print Method for `seroincidence.est` Object
#'
#' @description
#' Custom [print()] function to show output of the seroincidence calculator [find_MLE()].
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
print.seroincidence.est <- function(x, ...)
{
  cat("`seroincidence.est` object estimated given the following setup:\n")
  cat(paste("a) `antigen_isos`: ", paste(attr(x, "antigen_isos"), collapse = ", ")), "\n")
  cat(paste("b) `lambda.start`: ", attr(x, "lambda.start"), "\n"))
  cat("Call the `summary()` function to obtain output results.\n")
  cat("Call the `plot()` function to graph the log-likelihood curve.\n")
  invisible(x)
}
