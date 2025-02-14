#' Calculate mean antibody response using 5-parameter model
#'
#' @param t time since last exposure
#' @param y0 antibody response at exposure
#' @param y1 antibody response at t1
#' @param t1 time from last exposure to peak response
#' @param alpha decay power function coefficient
#' @param shape decay power function exponent
#' [numeric] scalar; uses r > 1 scale for shape
#'
#' @returns a [numeric] [vector]
#' @export
#'
#' @examples
#' params <- typhoid_curves_nostrat_100[1, ]
#' ab_5p(
#'   t = units::as_units(50, "days"),
#'   y0 = params$y0,
#'   y1 = params$y1,
#'   t1 = params$t1 |> units::as_units("days"),
#'   alpha = params$alpha |> units::as_units("1/days"),
#'   shape = params$r)
ab_5p <- function(t, y0, y1, t1, alpha, shape) {
  beta <- bt(y0, y1, t1)

  yt <- if_else(
    t <= t1,
    ab_5p_active_phase(t, y0, beta),
    ab_5p_decay_phase(t, t1, y1, alpha, shape)
  )
  return(yt)
}

ab_5p_active_phase <- function(t, y0, beta) {
  yt <- y0 * exp(units::set_units(beta * t, NULL))
  return(yt)
}

ab_5p_decay_phase <- function(
    t,
    t1,
    y1,
    alpha,
    shape) {
  base_part_1 <- y1^(1 - shape)
  base_part_2 <- units::set_units(
    x = (1 - shape) * alpha * (t - t1),
    value = NULL
  )
  base <- base_part_1 - base_part_2
  expt <- (1 / (1 - shape))
  ans <- base^expt
  return(ans)
}
