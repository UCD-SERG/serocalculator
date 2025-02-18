#' Calculate time to end of active infection
#'
#' @inheritParams antibody_decay_curve
#' @returns a [numeric()] vector
#' @export
#' @keywords internal
#' @examples
#' t1f(
#'   y0 = 0.74916052,
#'   b0 = 1,
#'   mu_b = 0.18432798,
#'   mu_y = 0.36853621,
#'   gamma = 0.0013040664)
t1f = function(
    y0 = 0.74916052, # taken from simpar run from vignette
    b0 = 1,
    mu_b = 0.18432798,
    mu_y = 0.36853621,
    gamma = 0.0013040664
  )
{
  t1_log_term = log(1 + (mu_y - mu_b) * b0 / (gamma * y0))
  t1 = t1_log_term / (mu_y - mu_b)
  return(t1)
}
