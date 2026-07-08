#' Quantize an inter-infection time to whole days
#'
#' Rounds a continuous inter-infection time (in days) to the nearest whole
#' day, with a floor of one day. The antibody-kinetics simulation in
#' `simresp.tinf()` already operates on a daily grid, but the number of
#' `while()`-loop iterations (and hence the number of random draws consumed)
#' otherwise depends on the platform-specific floating-point result of
#' `log()`. Quantizing to whole days keeps random-number consumption
#' identical across operating systems, so simulated data and its snapshots
#' are reproducible on macOS, Windows, and Linux.
#'
#' The floor of one day prevents a zero-length step, which would otherwise
#' stall the `while()` loop in `simresp.tinf()`.
#'
#' @param t_next a positive [numeric] scalar inter-infection time, in days
#' @returns a whole-number-valued [numeric] scalar, at least 1
#' @noRd
quantize_t_next <- function(t_next) {
  max(1, round(t_next))
}
