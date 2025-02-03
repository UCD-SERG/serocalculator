#' collect cross-sectional data
#'
#' @param lambda seroconversion rate (in events/person-day)
#' @param n_samples
#' number of samples n_samples (= nr of simulated records)
#' @param age_range age range to use for simulating data, in days
#' @param age_fixed age_fixed for parameter sample
#' (age_fixed = NA for age at infection)
#' @param antigen_isos
#' [character] [vector] with one or more antibody names.
#' Values must match `curve_params`.
#' @param n_mcmc_samples
#' * when `n_mcmc_samples` is in 1:4000,
#' a fixed posterior sample is used
#' * when n_mcmc_samples = 0 a random sample is chosen
#' @param renew_params
#' * `renew_params = TRUE`
#' generates a new parameter set for each infection
#' * `renew_params = FALSE`
#' keeps the one selected at birth, but updates baseline y0
#' @inheritDotParams simresp.tinf -t_end
#'
#' @returns an [array()] with dimensions
#' `n_samples`, `length(antigen_isos) + 1`,
#' where rows are observations and columns are age and biomarkers y(t)
#' @keywords internal
simcs.tinf <- function(# nolint: object_name_linter
    lambda,
    n_samples,
    age_range,
    age_fixed = NA,
    antigen_isos,
    n_mcmc_samples = 0,
    renew_params = FALSE,
    ...) {
  start_days <- round(age_range[1])
  # from min=age_range[1] days...
  end_days <- round(age_range[2])
  # to   max=age_range[2] days...
  if (start_days == 0) {
    start_days <- 1
  }

  y_sample <- array(
    NA,
    dim = c(n_samples, length(antigen_isos) + 1),
    dimnames = list(
      obs = 1:n_samples,
      var = c("age", antigen_isos)
    )
  )
  # y and age
  for (cur_sample in 1:n_samples) {
    resp <-
      simresp.tinf(
        lambda,
        t_end = end_days,
        age_fixed = age_fixed,
        antigen_isos = antigen_isos,
        n_mcmc_samples = n_mcmc_samples,
        renew_params = renew_params,
        ...
      )
    # pick an age at random
    age_index <-
      sample((start_days:end_days), size = 1)
    # sample at randomly chosen age
    y_sample[cur_sample, ] <-
      c(resp$t[age_index], as.matrix(resp$y)[age_index, ])
  }
  return(y_sample)
}
