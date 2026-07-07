#' Simulate time since last seroconversion for a population sample
#'
#' @description
#' Internal helper for [sim_pop_data_2()]. Samples each individual's time
#' since their last seroconversion from an exponential distribution with
#' rate `lambda`, capped at that individual's `age` (never-infected
#' individuals get an infinite time since last seroconversion).
#'
#' @param lambda a [numeric()] (or `units`-classed) scalar incidence rate
#' @param n_samples number of individuals to simulate
#' @param age a [numeric()] (or `units`-classed) vector of each individual's
#' age, used to cap the simulated time (in the same units as `1 / lambda`)
#'
#' @return a [numeric()] vector of length `n_samples`, in the same units as
#' `1 / lambda` if `lambda` is `units`-classed
#'
#' @keywords internal
#' @noRd
sim_time_since_last_sc <- function(lambda, n_samples, age) {

  t <- rexp(
    n = n_samples,
    rate = lambda
  )

  if (inherits(lambda, "units")) {
    t_units <- units(1 / lambda)
    t <- t |>
      units::as_units(t_units)
    extra_val <- units::as_units(Inf, t_units)
  } else {
    extra_val <- Inf
  }

  t <- if_else(t > age, extra_val, t)

  return(t)
}
