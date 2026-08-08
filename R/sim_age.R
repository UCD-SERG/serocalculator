#' Simulate ages for a population sample
#'
#' @description
#' Internal helper for [sim_pop_data_2()]. Samples `n_samples` ages
#' uniformly from `age_range`.
#'
#' @param n_samples number of ages to simulate
#' @param age_range a length-2 [numeric()] (or `units`-classed) vector giving
#' the lower and upper bounds to sample from
#'
#' @return a [numeric()] vector of length `n_samples`, in the same units as
#' `age_range` if `age_range` is `units`-classed
#'
#' @keywords internal
#' @noRd
sim_age <- function(n_samples, age_range) {

  age <- runif(n_samples,
               min = age_range[1],
               max = age_range[2])

  if (inherits(age_range, "units")) {
    age <- age |> units::as_units(units(age_range))
  }

  return(age)
}
