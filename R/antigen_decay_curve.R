#' Pathogen decay curve
#'
#' @inheritParams antibody_decay_curve
#' @returns a [numeric()]
#' @export
#'
#' @examples
#' library(ggplot2);
#' ggplot() + geom_function(
#' fun = pathogen_decay_curve,
#' args = list(
#'   y0 = 0.74916052, # taken from simpar run from vignette
#'   b0 = 1,
#'   mu_b = 0.18432798,
#'   mu_y = 0.36853621,
#'   gamma = 0.0013040664)) + xlim(0, 100)
pathogen_decay_curve = function(
    t,
    y0 = 0.74916052, # taken from simpar run from vignette
    b0 = 1,
    mu_b = 0.18432798,
    mu_y = 0.36853621,
    gamma = 0.0013040664)
{
  fraction = gamma * y0 * (exp(mu_y * t) - exp(mu_b*t)) / (mu_y - mu_b)
  bt_active = (b0 * exp(mu_b * t)) - fraction

  bt = pmax(0, bt_active)
}
