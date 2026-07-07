sim_age <- function(n_samples, age_range) {

  age <- runif(n_samples,
               min = age_range[1],
               max = age_range[2])

  if (inherits(age_range, "units")) {
    age <- age |> units::as_units(units(age_range))
  }

  return(age)
}
