#' Calculate the decay-phase (post-peak) antibody response for [ab_5p()]
#'
#' @param t time since last exposure
#' @param t1 time from last exposure to peak response
#' @param y1 antibody response at t1
#' @param alpha decay power function coefficient
#' @param shape decay power function exponent
#'
#' @return a [numeric()] vector
#' @keywords internal
#' @noRd
.ab_5p_decay_phase <- function(
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
