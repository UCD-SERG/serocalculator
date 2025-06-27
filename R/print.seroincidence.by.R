#' @title
#' Print Method for `seroincidence.by` Object
#'
#' @description
#' Custom [print()] function for `seroincidence.by` objects
#' (from [est_seroincidence_by()])
#'
#' @param x A list containing output of function [est_seroincidence_by()].
#' @param ... Additional arguments affecting the summary produced.
#' @inherit print.seroincidence return
#' @examples
#' library(dplyr)
#'
#' xs_data <-
#'   sees_pop_data_pk_100
#'
#' curve <-
#'   typhoid_curves_nostrat_100 |>
#'   filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))
#'
#' noise <-
#'   example_noise_params_pk
#'
#' # estimate seroincidence
#' est2 <- est_seroincidence_by(
#'   strata = c("catchment"),
#'   pop_data = xs_data,
#'   curve_params = curve,
#'   noise_params = noise,
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#'   # num_cores = 8 # Allow for parallel processing to decrease run time
#' )
#'
#' # calculate summary statistics for the seroincidence object
#' print(est2)
#'
#' @export
#' @keywords internal
print.seroincidence.by <- function(x, ...) {
  cat("`seroincidence.by` object estimated given the following setup:\n")
  cat("a) Antigen isotypes   :",
      paste(attr(x, "antigen_isos"), collapse = ", "),
      "\n")
  cat("b) Strata       :",
      paste(attr(x, "Strata") |> attr("strata_vars"), collapse = ", "),
      "\n")

  cat("\n")
  cat("This object is a list of `seroincidence` objects,",
      "with added meta-data attributes:\n")
  cat("`antigen_isos` -",
      "Character vector of antigen isotypes used in analysis.\n")
  cat("`Strata`       -",
      "Input parameter strata of function `est_seroincidence_by()`\n")
  cat("\n")
  cat("Call the `summary()` function to obtain output results.\n")
  invisible(x)
}
