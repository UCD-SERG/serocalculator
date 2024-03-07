#' Calculate peak antibody concentration
#' @inheritParams antibody_decay_curve
#' @inheritDotParams t1f
#' @returns a [numeric()] vector
#' @export
#'
#' @examples
#' y1f(y0 = 1, mu_y = 1, t1 = 10)
y1f = function(
    y0 = 0.74916052,
    mu_y = 0.36853621,
    t1 = t1f(y0 = y0, mu_y = mu_y,...),
    ...)
{
  y0 * exp(mu_y * t1)
}
