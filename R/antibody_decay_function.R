#' Graph antibody decay function with natural parameters
#'
#' @param t
#' @param y0
#' @param b0
#' @param mu_b
#' @param mu_y
#' @param gamma
#' @param alpha
#' @param rho
#'
#' @return
#' @export
#'
#' @examples
#' ggplot() + geom_function(fun = antibody_decay) + xlim(1,100)
antibody_decay = function(
    t,
    y0 = 0.74916052, # taken from simpar run from vignette
    b0 = 1,
    mu_b = 0.18432798,
    mu_y = 0.36853621,
    gamma = 0.0013040664,
    alpha = 0.00002192627,
    # rho = 2.9621994
    rho = 2 # exponential decay?
    )
{
  t1_log_term = log(1 + (mu_y - mu_b) * b0 / (gamma * y0))
  t1 = t1_log_term / (mu_y - mu_b)

  bt_active = b0 * exp(mu_b * t) - (gamma * y0 * (exp(mu_y * t) - exp(mu_b*t)) / (mu_y - mu_b))

  bt = pmax(0, bt_active)

  y1 = y0 * exp(mu_y * t1)

  yt = ifelse(
    t < t1,
    y0 * exp(mu_y * t),
    y1 * (1+ (rho-1) * (y1 ^ (rho - 1)) * alpha * (t - t1))^(-1/(rho - 1))
  )
  return(yt)
}
