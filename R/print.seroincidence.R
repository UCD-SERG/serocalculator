#' @title
#' Print Method for `seroincidence` Object
#'
#' @description
#' Custom [print()] function to show output of the seroincidence calculator [est.incidence()].
#'
#' @param x A list containing output of function [est.incidence.by()].
#' @param ... Additional arguments affecting the summary produced.
#' @return A \code{list} containing the output of the function \code{est.incidence()}.
#'
#' @examples
#'
#' \donttest{
#'
#' library(tidyverse)
#'
#' xs_data <- load_pop_data("https://osf.io/download//n6cp3/") %>%
#'   clean_pop_data()
#'
#' curve <- load_curve_params("https://osf.io/download/rtw5k/") %>%
#'   filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) %>%
#'   slice(1:100, .by = antigen_iso) # Reduce dataset for the purposes of this example
#'
#' noise <- load_noise_params("https://osf.io/download//hqy4v/")
#'
#'
#' est1 <- est.incidence(
#'   pop_data = xs_data %>% filter(Country == "Pakistan"),
#'   curve_param = curve,
#'   noise_param = noise %>% filter(Country == "Pakistan"),
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA")
#' )
#'
#' print(est1)
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
