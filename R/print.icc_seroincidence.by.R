#' Print method for stratified ICC results
#'
#' @param x an object of class `icc_seroincidence.by`
#' @param ... unused
#' @return invisible x
#' @export
print.icc_seroincidence.by <- function(x, ...) {
  cli::cli_h1("Intraclass Correlation Coefficient (ICC) by Stratum")
  cli::cli_text("")

  # Print as a data frame
  print(tibble::as_tibble(x))

  invisible(x)
}
