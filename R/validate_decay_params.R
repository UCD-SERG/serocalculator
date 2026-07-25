#' Validate decay-curve model parameters
#'
#' @description
#' Internal helper to validate the natural parameters shared by
#' [antibody_decay_curve()], [pathogen_decay_curve()], [t1f()], and [y1f()].
#'
#' @param y0 initial antibody concentration
#' @param b0 initial bacteria concentration
#' @param mu_b pathogen reproduction rate
#' @param mu_y antibody reproduction rate
#' @param gamma bacteria destruction rate per antibody
#'
#' @returns invisible `NULL`; called for its side effect of raising an
#' error if any parameter is invalid.
#'
#' @keywords internal
#' @noRd
.validate_decay_params <- function(y0, b0, mu_b, mu_y, gamma) {
  params <- c(y0 = y0, b0 = b0, mu_b = mu_b, mu_y = mu_y, gamma = gamma)
  negative_params <- params[params < 0]

  if (length(negative_params) > 0) {
    cli::cli_abort(
      c(
        "Decay-curve parameters must be non-negative.",
        "x" = "{.arg {names(negative_params)}} = {.val {negative_params}}"
      )
    )
  }

  if (isTRUE(mu_y == mu_b)) {
    cli::cli_abort(
      "{.arg mu_y} must differ from {.arg mu_b} to avoid division by zero."
    )
  }

  invisible(NULL)
}
