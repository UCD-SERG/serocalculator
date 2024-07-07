#' @title
#' Print Method for Seroincidence Summary Object
#'
#' @description
#' Custom [print()] function for "summary.seroincidence.by" objects (constructed by [summary.seroincidence.by()])
#'
#' @param x A "summary.seroincidence.by" object (constructed by [summary.seroincidence.by()])
#' @param ... Additional arguments affecting the summary produced.
#' @return A \code{list} containing the summary statistics for output of the function \code{est.incidence.by()}.
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
#' est2 <- est.incidence.by(
#'   strata = c("catchment"),
#'   pop_data = xs_data %>% filter(Country == "Pakistan"),
#'   curve_params = curve,
#'   noise_params = noise %>% filter(Country == "Pakistan"),
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#'   #num_cores = 8 # Allow for parallel processing to decrease run time
#' )
#'
#' summary(est2) %>%
#' print()}
#'
#' @export
print.summary.seroincidence.by <- function(x, ...)
{
  cat("Seroincidence estimated given the following setup:\n")
  cat(paste("a) Antigen isotypes   :", paste(x %>% attr("antigen_isos"), collapse = ", ")), "\n")
  cat(paste("b) Strata       :", paste(x %>% attr("Strata"), collapse = ", ")), "\n")
  cat("\n Seroincidence estimates:\n")
  print(as_tibble(x))
  invisible(x)
}
