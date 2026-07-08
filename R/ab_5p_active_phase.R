#' Calculate the active-phase (rising) antibody response for [ab_5p()]
#'
#' @param t time since last exposure
#' @param y0 antibody response at exposure
#' @param beta exponential growth rate, from `bt()`
#'
#' @return a [numeric()] vector
#' @keywords internal
#' @noRd
.ab_5p_active_phase <- function(t, y0, beta) { # nolint: object_name_linter
  yt <- y0 * exp(units::set_units(beta * t, NULL))
  return(yt)
}
