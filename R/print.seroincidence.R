#' @title
#' Print Method for `seroincidence` Object
#'
#' @description
#' Custom [print()] function to show output of the seroincidence calculator [est.incidence()].
#'
#' @param x A list containing output of function [est.incidence.by()].
#' @param ... Additional arguments affecting the summary produced.
#' @returns an [invisible] copy of input parameter `x`
#' @examples
#' library(dplyr)
#'
#' xs_data <-
#'   sees_pop_data_pk_100
#'
#' curve <-
#'   typhoid_curves_nostrat_100 %>%
#'   filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))
#'
#' noise <-
#'   example_noise_params_pk
#'
#' est1 <- est.incidence(
#'   pop_data = xs_data,
#'   curve_params = curve,
#'   noise_params = noise,
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#' )
#' print(est1)
#'
#' @export
#' @keywords internal
print.seroincidence <- function(x, ...) {
  cat("`seroincidence` object estimated given the following setup:\n")
  cat(paste("a) `antigen_isos`: ", paste(attr(x, "antigen_isos"), collapse = ", ")), "\n")
  cat(paste("b) `lambda_start`: ", attr(x, "lambda_start"), "\n"))
  cat("Call the `summary()` function to obtain output results.\n")
  cat("Call the `autoplot()` function to graph the log-likelihood curve.\n")
  invisible(x)
}
