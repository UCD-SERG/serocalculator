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
  } else extra_val <- Inf

  t <- if_else(t > age, extra_val, t)

  return(t)
}
