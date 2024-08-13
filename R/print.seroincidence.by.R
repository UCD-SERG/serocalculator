#' @title
#' Print Method for `seroincidence.by` Object
#'
#' @description
#' Custom [print()] function to show output of the seroincidence calculator [est.incidence.by()].
#'
#' @param x A list containing output of function [est.incidence.by()].
#' @param ... Additional arguments affecting the summary produced.
#' @return A \code{list} containing the output of the function \code{est.incidence.by()}.
#'
#' @examples
#'
#' \donttest{
#'
#' library(tidyverse)
#'
#' xs_data <- load_pop_data("https://osf.io/download//n6cp3/")
#'
#' curve <- load_curve_params("https://osf.io/download/rtw5k/") %>%
#'   filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) %>%
#'   slice(1:100, .by = antigen_iso) # Reduce dataset for the purposes of this example
#'
#' noise <- load_noise_params("https://osf.io/download//hqy4v/")
#'
#'
#' est2 <- est.incidence.by(
#'   strata = c("catchment"),
#'   pop_data = xs_data %>% filter(Country == "Pakistan"),
#'   curve_params = curve,
#'   noise_params = noise %>% filter(Country == "Pakistan"),
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#'   #num_cores = 8 # Allow for parallel processing to decrease run time
#' )
#'
#' print(est2)
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
