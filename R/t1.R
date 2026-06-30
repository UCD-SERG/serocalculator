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
#'   gamma = 0.0013040664
#' )
t1f <- function( # nolint: object_name_linter
    y0 = 0.74916052,
    b0 = 1,
    mu_b = 0.18432798,
    mu_y = 0.36853621,
    gamma = 0.0013040664) {
  .validate_decay_params( # nolint: object_usage_linter
    y0 = y0, b0 = b0, mu_b = mu_b, mu_y = mu_y, gamma = gamma
  )

  log_arg <- 1 + (mu_y - mu_b) * b0 / (gamma * y0)

  if (log_arg <= 0) {
    cli::cli_abort(
      c(
        "Cannot compute {.fn t1f}: the argument of {.fn log} must be positive.",
        "x" = "{.code 1 + (mu_y - mu_b) * b0 / (gamma * y0)} = {.val {log_arg}}"
      )
    )
  }

  t1_log_term <- log(log_arg)
  t1 <- t1_log_term / (mu_y - mu_b)
  return(t1)
}
