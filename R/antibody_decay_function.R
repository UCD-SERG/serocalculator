#' Antibody decay curve with natural parameters
#'
#' @param b0 initial bacteria concentration
#' @param y0 initial antibody concentration
#' @param mu_y antibody reproduction rate
#' @param mu_b pathogen reproduction rate
#' @param gamma bacteria destruction rate per antibody
#' @param t time since infection
#' @param alpha antibody decay rate
#' @param rho antibody decay rate power function parameter
#'
#' @returns a [numeric()] vector
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot() + geom_function(fun = antibody_decay_curve) + xlim(1, 100)
antibody_decay_curve <- function(
    t,
    y0 = 0.74916052,
    b0 = 1,
    mu_b = 0.18432798,
    mu_y = 0.36853621,
    gamma = 0.0013040664,
    alpha = 0.00002192627,
    rho = 2) {
  .validate_decay_params( # nolint: object_usage_linter
    y0 = y0, b0 = b0, mu_b = mu_b, mu_y = mu_y, gamma = gamma
  )
  if (alpha < 0) {
    cli::cli_abort("{.arg alpha} must be non-negative.")
  }
  if (rho < 1) {
    cli::cli_abort("{.arg rho} must be >= 1.")
  }

  t1 <- t1f( # nolint: object_usage_linter
    b0 = b0,
    y0 = y0,
    mu_y = mu_y,
    mu_b = mu_b,
    gamma = gamma
  )

  y1 <- y1f( # nolint: object_usage_linter
    y0 = y0,
    mu_y = mu_y,
    t1 = t1
  )

  # pmax() keeps the base of the fractional power non-negative for t < t1,
  # where this branch's value is discarded anyway (see ifelse() below) -
  # avoids spurious "NaNs produced" warnings from raising a negative base
  # to a non-integer power.
  phase2_rho1 <- y1 * exp(-alpha * (t - t1))
  phase2_exponent <- -1 / (rho - 1)
  phase2_base <-
    1 + (rho - 1) * (y1 ^ (rho - 1)) * alpha * pmax(t - t1, 0)
  phase2_default <- y1 * phase2_base ^ phase2_exponent
  yt <- ifelse(
    t < t1,
    y0 * exp(mu_y * t),
    ifelse(
      rho == 1,
      phase2_rho1,
      phase2_default
    )
  )
  return(yt)
}
