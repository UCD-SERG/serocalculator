#' @title
#' Print Method for `seroincidence.by` Object
#'
#' @description
#' Custom [print()] function to show output of the seroincidence calculator [est.incidence.by()].
#'
#' @param x A list containing output of function [est.incidence.by()].
#' @param ... Additional arguments affecting the summary produced.
#' @returns Prints a list containing output of function [est.incidence.by()].
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
print.seroincidence.by <- function(x, ...) {
  cat("`seroincidence.by` object estimated given the following setup:\n")
  cat(paste("a) Antigen isotypes   :", paste(attr(x, "antigen_isos"), collapse = ", ")), "\n")
  cat(paste("b) Strata       :", paste(attr(x, "Strata") %>% attr("strata_vars"), collapse = ", ")), "\n")

  cat("\n")
  cat("This object is a list of `seroincidence` objects, with added meta-data attributes:")
  cat("`antigen_isos`   - Character vector of antigen isotypes used in analysis.\n")
  cat("`Strata`       - Input parameter strata of function `est.incidence.by()`\n")
  cat("\n")
  cat("Call the `summary()` function to obtain output results.\n")
  invisible(x)
}
