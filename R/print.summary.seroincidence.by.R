#' @title
#' Print Method for Seroincidence Summary Object
#'
#' @description
#' Custom [print()] function for "summary.seroincidence.by" objects
#' (constructed by [summary.seroincidence.by()])
#'
#' @param x A "summary.seroincidence.by" object
#' (constructed by [summary.seroincidence.by()])
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
#' est2 <- estimate_scr_by(
#'   strata = c("catchment"),
#'   pop_data = xs_data,
#'   curve_params = curve,
#'   noise_params = noise,
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#'   # num_cores = 8 # Allow for parallel processing to decrease run time
#' )
#'
#' # calculate summary statistics for the seroincidence object
#' est2_summary <- summary(est2)
#' print(est2_summary)
#'
#' @export
#' @keywords internal
print.summary.seroincidence.by <- function(x, ...) {
  cat("Seroincidence estimated given the following setup:\n")
  cat("a) Antigen isotypes   :",
      paste(x |> attr("antigen_isos"), collapse = ", "),
      "\n")
  cat("b) Strata       :",
      paste(x |> attr("Strata"), collapse = ", "),
      "\n")
  cat("\n Seroincidence estimates:\n")
  print(as_tibble(x))
  invisible(x)
}
