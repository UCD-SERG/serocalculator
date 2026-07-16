#' Calculate peak antibody concentration
#' @inheritParams antibody_decay_curve
#' @inheritDotParams t1f
#' @returns a [numeric()] vector
#' @export
#' @keywords internal
#' @examples
#' y1f(y0 = 1, mu_y = 1, t1 = 10)
y1f <- function( # nolint: object_name_linter
    y0 = 0.74916052,
    mu_y = 0.36853621,
    t1 = t1f(y0 = y0, mu_y = mu_y, ...), # nolint: object_usage_linter
    ...) {
  if (y0 < 0) {
    cli::cli_abort("{.arg y0} must be non-negative.")
  }
  if (mu_y < 0) {
    cli::cli_abort("{.arg mu_y} must be non-negative.")
  }

  y0 * exp(mu_y * t1)
}
