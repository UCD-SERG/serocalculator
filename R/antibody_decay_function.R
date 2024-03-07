#' Graph antibody decay function with natural parameters
#'
#' @param b0 initial bacteria concentration
#' @param y0 initial antibody concnetration
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
#' ggplot() + geom_function(fun = antibody_decay_curve) + xlim(1,100)
antibody_decay_curve = function(
    t,
    y0 = 0.74916052, # taken from simpar run from vignette
    b0 = 1,
    mu_b = 0.18432798,
    mu_y = 0.36853621,
    gamma = 0.0013040664,
    alpha = 0.00002192627,
    rho = 2
    # rho = 2 # exponential decay?
    )
{

  t1 = t1f(
    b0 = b0,
    y0 = y0,
    mu_y = mu_y,
    mu_b = mu_b,
    gamma = gamma
  )

  y1 = y1f(
    y0 = y0,
    mu_y = mu_y,
    t1 = t1)

  yt = ifelse(
    t < t1,
    y0 * exp(mu_y * t),
    y1 * (1+ (rho-1) * (y1 ^ (rho - 1)) * alpha * (t - t1))^(-1/(rho - 1))
  )
  return(yt)
}
